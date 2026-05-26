/* Africa AEZ Yield-Frontier Explorer */

const COLOR_LOW  = [178,  24,  43];   // #b2182b
const COLOR_MID  = [247, 247, 247];   // #f7f7f7
const COLOR_HIGH = [ 27, 120,  55];   // #1b7837

const fmtPct = (v, dec = 0) =>
  v == null || isNaN(v) ? "—" : (v * 100).toFixed(dec) + "%";
const fmtNum = (v, dec = 2) =>
  v == null || isNaN(v) ? "—" : Number(v).toFixed(dec);
const fmtArea = (ha) => {
  if (ha == null) return "—";
  if (ha >= 1e6) return (ha / 1e6).toFixed(2) + " M ha";
  if (ha >= 1e3) return (ha / 1e3).toFixed(0) + " k ha";
  return ha.toFixed(0) + " ha";
};
const fmtUSD = (v) => {
  if (v == null || isNaN(v)) return "—";
  if (v >= 1e9) return "$" + (v / 1e9).toFixed(2) + " B";
  if (v >= 1e6) return "$" + (v / 1e6).toFixed(0) + " M";
  if (v >= 1e3) return "$" + (v / 1e3).toFixed(0) + " k";
  return "$" + v.toFixed(0);
};

function lerp(a, b, t) { return a + (b - a) * t; }
function lerpRGB(c1, c2, t) {
  return [Math.round(lerp(c1[0], c2[0], t)),
          Math.round(lerp(c1[1], c2[1], t)),
          Math.round(lerp(c1[2], c2[2], t))];
}

/* Diverging colour: 0 → low, midpoint → mid, 1 → high. Squish above 1. */
function colorFor(perf, midpoint) {
  if (perf == null || isNaN(perf)) return "#dddddd";
  const v = Math.max(0, Math.min(1, perf));
  let rgb;
  if (v <= midpoint) {
    const t = midpoint > 0 ? v / midpoint : 0;
    rgb = lerpRGB(COLOR_LOW, COLOR_MID, t);
  } else {
    const t = (v - midpoint) / (1 - midpoint);
    rgb = lerpRGB(COLOR_MID, COLOR_HIGH, t);
  }
  return `rgb(${rgb[0]},${rgb[1]},${rgb[2]})`;
}

/* ---------- State ---------- */
const state = {
  details: null,    // zone_id (str) -> details object
  meta: null,
  zoneLayers: {},   // zone_id (str) -> Leaflet layer
  selectedId: null,
};

/* ---------- Map setup ---------- */
const map = L.map("map", {
  zoomControl: true,
  zoomSnap: 0.25,
  preferCanvas: true,
  attributionControl: false,
});
map.setView([2, 18], 3);

const baseStyle = {
  color: "#888",
  weight: 0.4,
  fillColor: "#f5f5f0",
  fillOpacity: 0.6,
};

/* ---------- Load and render ---------- */
Promise.all([
  fetch("data/zones.geojson").then(r => r.json()),
  fetch("data/africa.geojson").then(r => r.json()),
  fetch("data/zone_details.json").then(r => r.json()),
  fetch("data/meta.json").then(r => r.json()),
]).then(([zones, africa, details, meta]) => {
  state.details = details;
  state.meta = meta;

  const africaLayer = L.geoJSON(africa, {
    style: baseStyle,
    interactive: false,
  }).addTo(map);

  const zoneLayer = L.geoJSON(zones, {
    style: (feat) => zoneStyle(feat, meta.midpoint),
    onEachFeature: (feat, layer) => {
      const id = String(feat.properties.zone_id);
      state.zoneLayers[id] = layer;
      layer.bindTooltip(buildTooltip(feat.properties), {
        sticky: true,
        direction: "top",
        offset: [0, -4],
        className: "zone-tooltip",
      });
      layer.on("click", () => selectZone(id, { fly: false }));
      layer.on("mouseover", () => {
        if (state.selectedId !== id) {
          layer.setStyle({ weight: 1.2, color: "#222" });
          layer.bringToFront();
        }
      });
      layer.on("mouseout", () => {
        if (state.selectedId !== id) {
          zoneLayer.resetStyle(layer);
        }
      });
    },
  }).addTo(map);

  map.fitBounds(zoneLayer.getBounds(), { padding: [10, 10] });

}).catch((err) => {
  console.error("Failed to load data:", err);
  document.getElementById("panel-empty").innerHTML =
    `<h2>Couldn't load data</h2>
     <p>Run <code>Rscript build-data.R</code> in <code>tool/</code> first,
     then serve this directory (e.g. <code>python -m http.server</code>).</p>`;
});

function zoneStyle(feat, midpoint) {
  return {
    fillColor: colorFor(feat.properties.composite_perf, midpoint),
    fillOpacity: 0.85,
    color: "#444",
    weight: 0.35,
  };
}

function buildTooltip(p) {
  return `
    <strong>${p.country_name || p.iso_a3} &middot; ${p.regime}</strong><br>
    ${p.label}<br>
    <span class="ttip-perf">Composite: ${fmtPct(p.composite_perf)}</span>
  `;
}

/* ---------- Selection ---------- */
function selectZone(id, opts = {}) {
  const detail = state.details[id];
  if (!detail) {
    console.warn("No details for zone", id);
    return;
  }
  // Reset previous selection to its computed base style
  if (state.selectedId && state.zoneLayers[state.selectedId]) {
    const prevLayer = state.zoneLayers[state.selectedId];
    const prevProps = prevLayer.feature.properties;
    prevLayer.setStyle(zoneStyle({ properties: prevProps }, state.meta.midpoint));
  }
  state.selectedId = id;

  const layer = state.zoneLayers[id];
  if (layer) {
    layer.setStyle({ color: "#000", weight: 2 });
    layer.bringToFront();
    if (opts.fly) {
      map.flyToBounds(layer.getBounds(), { padding: [40, 40], duration: 0.6 });
    }
  }

  renderPanel(detail);
}

/* ---------- Panel rendering ---------- */
function renderPanel(d) {
  document.getElementById("panel-empty").hidden = true;
  document.getElementById("panel-content").hidden = false;

  document.getElementById("p-country").textContent =
    d.country_name || d.iso_a3 || "—";
  document.getElementById("p-zone-tag").textContent =
    `${d.zone_label}  ·  zone #${d.zone_id}`;
  document.getElementById("p-regime").textContent = d.regime || "—";
  document.getElementById("p-cluster").textContent = d.label || "—";
  document.getElementById("p-area").textContent = fmtArea(d.zone_total_area_ha);

  const perfEl = document.getElementById("p-perf");
  perfEl.textContent = fmtPct(d.composite_perf, 0);
  perfEl.style.color = colorFor(d.composite_perf, state.meta.midpoint);

  const blurb = document.getElementById("p-blurb");
  if (d.description) {
    blurb.textContent = d.description;
    blurb.style.display = "";
  } else {
    blurb.style.display = "none";
  }

  const tbody = document.querySelector("#p-crops tbody");
  tbody.innerHTML = "";
  const crops = d.crops || [];
  if (crops.length === 0) {
    tbody.innerHTML = `<tr><td colspan="6" style="color:#999;font-style:italic">
      No crops with sufficient peer coverage in this cluster.</td></tr>`;
    return;
  }

  for (const c of crops) {
    const tr = document.createElement("tr");
    const ratio = c.frontier_ratio;
    const ratioPct = ratio == null ? 0 : Math.max(0, Math.min(1, ratio));
    tr.innerHTML = `
      <td>${escapeHTML(c.crop)}</td>
      <td class="num">${fmtArea(c.area_ha)}</td>
      <td class="num">${fmtNum(c.yield_t_ha, 2)}</td>
      <td class="num">${fmtNum(c.frontier_yield_p95, 2)}</td>
      <td class="num ratio-cell">
        <span class="ratio-bar"><span style="width:${(ratioPct*100).toFixed(0)}%"></span></span>
        ${fmtPct(ratio, 0)}
      </td>
      <td>${frontierCell(c)}</td>
    `;
    tbody.appendChild(tr);
  }

  // Potential-value table
  const ptbody = document.querySelector("#p-potential tbody");
  ptbody.innerHTML = "";
  const potcrops = d.potential_crops || [];
  if (potcrops.length === 0) {
    ptbody.innerHTML = `<tr><td colspan="6" style="color:#999;font-style:italic">
      No GAEZ potential data for this zone.</td></tr>`;
    return;
  }
  for (const c of potcrops) {
    const tr = document.createElement("tr");
    const ach = c.achievement_crop;
    const achPct = ach == null ? 0 : Math.max(0, Math.min(1, ach));
    tr.innerHTML = `
      <td>${escapeHTML(c.crop)}</td>
      <td class="num">${fmtNum(c.current_yield_t_ha, 2)}</td>
      <td class="num"><strong>${fmtNum(c.potential_yield_t_ha, 2)}</strong></td>
      <td class="num">$${fmtNum(c.price_usd_per_tonne, 0)}</td>
      <td class="num"><strong>${fmtUSD(c.potential_usd)}</strong></td>
      <td class="num ratio-cell">
        <span class="ratio-bar"><span style="width:${(achPct*100).toFixed(0)}%"></span></span>
        ${fmtPct(ach, 0)}
      </td>
    `;
    ptbody.appendChild(tr);
  }
}

function frontierCell(c) {
  if (c.is_self_frontier) {
    return `<span class="self-frontier">this zone</span>`;
  }
  const id = String(c.frontier_zone_id);
  const country = c.frontier_country || c.frontier_iso || "—";
  const regimeId = c.frontier_regime_id != null ? `·${c.frontier_regime_id}` : "";
  return `<a class="frontier-link" data-zone="${id}" href="#zone-${id}">
    ${escapeHTML(country)}<span style="color:#9a9a9a">${regimeId}</span>
  </a>`;
}

function escapeHTML(s) {
  return String(s == null ? "" : s)
    .replace(/&/g, "&amp;").replace(/</g, "&lt;")
    .replace(/>/g, "&gt;").replace(/"/g, "&quot;");
}

document.addEventListener("click", (e) => {
  const a = e.target.closest(".frontier-link");
  if (!a) return;
  e.preventDefault();
  const id = a.getAttribute("data-zone");
  if (id) selectZone(id, { fly: true });
});
