/* ============================================================================
 * basemap.js — offline vector basemap for the "Karte" tab
 * ============================================================================
 * Draws the map's background from Natural Earth GeoJSON that the app serves
 * itself (www/basemap/, built by build_basemap.R). Nothing here is fetched from
 * a third party, so using the map hands no visitor's IP address to an outside
 * host — which is why the atlas draws its own basemap rather than loading raster
 * tiles from a map CDN.
 *
 * The look follows the CARTO Positron style: an unstroked land fill with country
 * and province lines as separate layers on top, and place names set in Open Sans.
 *
 * Entry point is window.atlasBasemap(map), which app.R calls from the leaflet
 * widget's onRender hook. The parsed GeoJSON is held in a module-level promise,
 * so the map re-renders that follow every filter change reuse it rather than
 * fetching and parsing the files again.
 * ========================================================================== */
(function () {
  "use strict";

  var ATTRIBUTION =
    'Kartengrundlage: <a href="https://www.naturalearthdata.com/" target="_blank" ' +
    'rel="noopener">Natural Earth</a> (Public Domain)';

  /* Colours of the Positron style. `water` also appears as the .leaflet-container
   * background in app.R's CSS, so the map is the right colour before this script
   * paints anything — keep the two in sync. Label colours and sizes live in that
   * same CSS block, on .atlas-place. */
  var STYLE = {
    water:       "#d4dadc",
    land:        "#fafaf8",
    border:      "#dfc3c6",  // country boundaries are pink in this style, not grey
    borderWidth: 0.8,
    admin1:      "#e2c6c8",
    admin1Width: 0.7,
    admin1Dash:  "3,2",
    admin1Zoom:  5           // provinces appear one zoom level after countries
  };

  var PANE_BASE   = "atlasBasemap";  // polygons: above tilePane (200), below overlays (400)
  var PANE_LABELS = "atlasLabels";   // place labels: still below the data overlays

  var cache = null;  // Promise<{land, lakes, borders, admin1, places}>, resolved once

  /* app.R loads this script as basemap.js?v=<stamp>, the stamp being the newest
   * modification time across the basemap files. Passing it on to the GeoJSON
   * requests means a rebuilt basemap reaches browsers that still hold the old one
   * in cache. document.currentScript is only readable while the script itself is
   * executing, hence capturing it here rather than inside the functions below. */
  var VERSION = (function () {
    var src = document.currentScript && document.currentScript.src;
    var m = src && src.match(/[?&]v=([^&]*)/);
    return m ? m[1] : "";
  })();

  /* The app may be served from a sub-path (e.g. /chikon/ on shinyapps.io).
   * Resolving against an asset the page already links keeps the basemap URLs
   * correct there as well as at the server root. */
  function assetBase() {
    var link = document.querySelector('link[rel="icon"][type="image/x-icon"]');
    if (link && link.href) return link.href.replace(/[^\/]*$/, "");
    return new URL(".", document.baseURI).href;
  }

  function load() {
    if (!cache) {
      var base = assetBase();
      var names = ["land", "lakes", "borders", "admin1", "places"];
      cache = Promise.all(names.map(function (name) {
        var url = base + "basemap/" + name + ".geojson" + (VERSION ? "?v=" + VERSION : "");
        return fetch(url).then(function (res) {
          if (!res.ok) throw new Error(url + " -> HTTP " + res.status);
          return res.json();
        });
      })).then(function (loaded) {
        var out = {};
        names.forEach(function (name, i) { out[name] = loaded[i]; });
        return out;
      });
      cache.catch(function () { cache = null; });  // allow a retry on the next render
    }
    return cache;
  }

  function escapeHtml(s) {
    return String(s).replace(/[&<>"']/g, function (ch) {
      return { "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;", "'": "&#39;" }[ch];
    });
  }

  function placeMarker(feature, lat, lng) {
    // `caps` marks the most prominent cities, which are set in capitals and in
    // the darker of the two label colours; build_basemap.R decides which. The
    // uppercasing is left to CSS so that the data keeps the real name.
    return L.marker([lat, lng], {
      pane: PANE_LABELS,
      interactive: false,
      keyboard: false,
      icon: L.divIcon({
        className: feature.properties.caps ? "atlas-place atlas-place--major" : "atlas-place",
        iconSize: null,
        html: '<i></i><span>' + escapeHtml(feature.properties.name) + '</span>'
      })
    });
  }

  /* Copy a FeatureCollection `deg` degrees east or west. Vector layers, unlike
   * tiles, do not repeat across the antimeridian, so without a copy either side
   * panning past 180° runs off the edge of the world into empty water. `addWorld`
   * builds a copy only once the view actually reaches that far. */
  function shiftGeoJSON(collection, deg) {
    function walk(c) {
      return typeof c[0] === "number" ? [c[0] + deg, c[1]] : c.map(walk);
    }
    return {
      type: "FeatureCollection",
      features: collection.features.map(function (f) {
        return {
          type: "Feature",
          properties: null,
          geometry: { type: f.geometry.type, coordinates: walk(f.geometry.coordinates) }
        };
      })
    };
  }

  window.atlasBasemap = function (map) {
    // The leaflet binding reuses a single map object across the re-renders that
    // every filter change triggers, and calls this hook once for it. The guard
    // keeps that safe either way: running twice would add every layer a second
    // time, drawing each boundary line on top of itself.
    if (map.__atlasBasemap) return;
    map.__atlasBasemap = true;

    if (map.attributionControl) map.attributionControl.addAttribution(ATTRIBUTION);

    // Both panes are decoration: aria-hidden keeps a screen reader from reading
    // out several hundred place names on top of the map's own description.
    var basePane = map.createPane(PANE_BASE);
    basePane.style.zIndex = 250;
    basePane.setAttribute("aria-hidden", "true");
    var labelPane = map.createPane(PANE_LABELS);
    labelPane.style.zIndex = 260;
    labelPane.style.pointerEvents = "none";
    labelPane.setAttribute("aria-hidden", "true");

    load().then(function (data) {
      // One canvas renderer for every polygon and line layer: far fewer DOM nodes
      // than SVG paths for ~1500 features, and it redraws smoothly while panning.
      var renderer = L.canvas({ pane: PANE_BASE, padding: 0.3 });
      // smoothFactor 0 draws the geometry exactly. The coastline has to hold to
      // the pixel, because app.R draws the same outlines again as shaded country
      // polygons on top, and leaflet's default path thinning can resolve one
      // coastline differently in each layer.
      var base = { pane: PANE_BASE, renderer: renderer, interactive: false, smoothFactor: 0 };
      var provinceLayers = [];
      var worlds = {};

      function geo(collection, style) {
        return L.geoJSON(collection, Object.assign({}, base, { style: style }));
      }

      // Draw order: an unstroked land fill, water bodies punched back out of it,
      // then the boundary lines on top.
      function addWorld(shift) {
        if (worlds[shift]) return;
        worlds[shift] = true;
        var at = function (c) { return shift ? shiftGeoJSON(c, shift) : c; };

        geo(at(data.land), { fillColor: STYLE.land, fillOpacity: 1, stroke: false }).addTo(map);
        geo(at(data.lakes), { fillColor: STYLE.water, fillOpacity: 1, stroke: false }).addTo(map);
        provinceLayers.push(geo(at(data.admin1), {
          color: STYLE.admin1, weight: STYLE.admin1Width, opacity: 1,
          dashArray: STYLE.admin1Dash, fill: false
        }));
        geo(at(data.borders), {
          color: STYLE.border, weight: STYLE.borderWidth, opacity: 1, fill: false
        }).addTo(map);
      }

      addWorld(0);

      // Labels are created on demand for what is actually on screen: each place
      // carries the lowest zoom at which it may appear, and only those inside the
      // current view get a marker. Without the viewport test the whole place file
      // would sit in the DOM from zoom 6 on, most of it far outside the frame.
      var labels = L.layerGroup().addTo(map);
      var shown = {};

      function refresh() {
        var zoom = map.getZoom();
        var view = map.getBounds().pad(0.15);  // slack, so labels do not pop at the edge
        var west = view.getWest(), east = view.getEast();

        // Give the view a neighbouring copy of the world as soon as it looks past
        // the edge of this one, so panning stays continuous the way tiles were.
        if (west < -180) addWorld(-360);
        if (east > 180) addWorld(360);

        provinceLayers.forEach(function (layer) {
          if (zoom >= STYLE.admin1Zoom) {
            if (!map.hasLayer(layer)) layer.addTo(map);
          } else if (map.hasLayer(layer)) {
            map.removeLayer(layer);
          }
        });

        var wanted = {};
        data.places.features.forEach(function (feature, i) {
          if (feature.properties.minzoom > zoom) return;
          var coords = feature.geometry.coordinates;
          var lng = coords[0];
          while (lng - 360 >= west) lng -= 360;   // leftmost copy at or after the west edge
          while (lng < west) lng += 360;
          for (; lng <= east; lng += 360) {       // a wide view can show a place twice
            var key = i + ":" + Math.round(lng);
            wanted[key] = true;
            if (!shown[key]) labels.addLayer(shown[key] = placeMarker(feature, coords[1], lng));
          }
        });
        Object.keys(shown).forEach(function (key) {
          if (!wanted[key]) {
            labels.removeLayer(shown[key]);
            delete shown[key];
          }
        });
      }

      map.on("moveend", refresh);  // fired after zooming as well as panning
      refresh();
    }).catch(function (err) {
      // A missing basemap must not take the markers down with it: the data
      // layers stay usable on the plain water-coloured background.
      console.error("Basemap konnte nicht geladen werden:", err);
    });
  };
})();
