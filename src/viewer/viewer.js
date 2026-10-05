// visilog viewer — the design's hierarchy as nested boxes, its values live,
// stepped through time. Talks to `visilog serve`; see src/serve.rs.
"use strict";

const $ = (selector) => document.querySelector(selector);
const SVG = "http://www.w3.org/2000/svg";
const elk = new ELK();

const state = {
  design: null,
  graph: null,
  signals: new Map(),      // id -> session SignalInfo
  graphSignals: new Map(), // id -> graph Signal
  instances: new Map(),    // id -> graph Instance
  values: {},              // id -> bits string (or number for a real)
  shownTime: 0,            // the time the diagram shows
  now: 0,                  // the simulation's own time
  folded: new Set(),
  expandedRegs: new Set(),
  pinned: [],
  traces: {},
  selected: null,          // { kind: "signal" | "instance", id }
  view: { x: 20, y: 20, k: 1 },
  bindings: new Map(),     // id -> [{ el, kind }]
  layout: null,
  consoleLength: 0,
  waveWindow: null,        // [t0, t1] or null to fit
  busy: false,
};

const REG_ROWS = 8;
const ROW = 16;
const HEAD = 26;
const PORT = 8;

// ---------------------------------------------------------------- formatting

function formatTime(ticks) {
  const fs = ticks * state.design.tick_femtoseconds;
  const units = [["s", 1e15], ["ms", 1e12], ["µs", 1e9], ["ns", 1e6], ["ps", 1e3], ["fs", 1]];
  for (const [name, scale] of units) {
    if (Math.abs(fs) >= scale || name === "fs") {
      const value = fs / scale;
      const text = Number.isInteger(value) ? value.toString() : value.toPrecision(6).replace(/\.?0+$/, "");
      return `${text} ${name}`;
    }
  }
}

function bitClass(bits) {
  if (typeof bits !== "string") return "vec";
  if (bits.length !== 1) return "vec";
  return { "0": "b0", "1": "b1", x: "bx", z: "bz" }[bits] || "vec";
}

/// A value for a label: one bit as itself, a vector as hex — with a digit that
/// is not fully known shown the way iverilog's %h does: lower case when every
/// bit agrees, upper case when they mix.
function formatValue(bits, radix = "hex") {
  if (bits === undefined || bits === null) return "–";
  if (typeof bits === "number") return Number.isInteger(bits) ? bits.toFixed(1) : bits.toPrecision(6);
  if (bits.length === 1) return bits;
  if (radix === "bin") return bits;
  if (/^[01]+$/.test(bits)) {
    const value = BigInt("0b" + bits);
    if (radix === "dec") return value.toString();
    return value.toString(16).toUpperCase().padStart(Math.ceil(bits.length / 4), "0");
  }
  if (/^x+$/.test(bits)) return "x";
  if (/^z+$/.test(bits)) return "z";
  let out = "";
  const pad = (4 - (bits.length % 4)) % 4;
  const padded = "0".repeat(pad) + bits;
  for (let i = 0; i < padded.length; i += 4) {
    const nibble = padded.slice(i, i + 4);
    if (/^[01]+$/.test(nibble)) out += parseInt(nibble, 2).toString(16).toUpperCase();
    else if (/^x+$/.test(nibble.slice(i === 0 ? pad : 0))) out += "x";
    else if (/^z+$/.test(nibble.slice(i === 0 ? pad : 0))) out += "z";
    else out += nibble.includes("x") ? "X" : "Z";
  }
  return out;
}

function signedDecimal(bits) {
  if (!/^[01]+$/.test(bits)) return "–";
  const value = BigInt("0b" + bits);
  return bits[0] === "1" ? (value - (1n << BigInt(bits.length))).toString() : value.toString();
}

function localName(id) {
  return id.slice(id.lastIndexOf(".") + 1);
}

// ------------------------------------------------------------------- network

async function api(path, body) {
  const response = await fetch(path, body === undefined ? {} : {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify(body),
  });
  if (!response.ok) throw new Error(await response.text());
  const type = response.headers.get("Content-Type") || "";
  return type.includes("json") ? response.json() : response.text();
}

// --------------------------------------------------------------------- setup

async function load(design) {
  state.design = design || await api("/api/design");
  const { graph } = state.design;
  state.graph = graph;
  state.signals = new Map(state.design.signals.map((s) => [s.id, s]));
  state.graphSignals = new Map(graph.signals.map((s) => [s.id, s]));
  state.instances = new Map(graph.instances.map((i) => [i.id, i]));
  state.processesByInstance = new Map();
  for (const process of graph.processes) {
    if (process.port_connection) continue;
    if (!state.processesByInstance.has(process.instance)) state.processesByInstance.set(process.instance, []);
    state.processesByInstance.get(process.instance).push(process);
  }
  $("#design-name").textContent = graph.top;
  document.title = `${graph.top} — visilog`;
  state.consoleLength = 0;
  $("#console").textContent = "";

  // Fold deep levels of a big design, so the first look is readable.
  if (state.folded.size === 0 && graph.instances.length > 12) {
    for (const instance of graph.instances) if (depth(instance.id) >= 2 && instance.children.length) state.folded.add(instance.id);
  }
  fillEdgeSignals();
  if (state.pinned.length === 0) autoPin();
  await refreshValues();
  await relayout(true);
  await refreshTraces();
  updateClock(state.design.state);
}

function depth(id) {
  let d = 0;
  let instance = state.instances.get(id);
  while (instance && instance.parent) { d++; instance = state.instances.get(instance.parent); }
  return d;
}

function isClockish(name) { return /(^|_)(clk|clock)|clk$|clock$/i.test(name); }

function fillEdgeSignals() {
  const select = $("#edge-signal");
  const ones = [...state.signals.values()].filter((s) => s.width === 1 && (s.kind === "net" || s.kind === "variable"));
  ones.sort((a, b) => (isClockish(b.name) - isClockish(a.name)) || a.id.split(".").length - b.id.split(".").length || a.id.localeCompare(b.id));
  select.innerHTML = "";
  for (const signal of ones) {
    const option = document.createElement("option");
    option.value = option.textContent = signal.id;
    select.append(option);
  }
}

function autoPin() {
  const top = state.graph.top;
  const own = [...state.signals.values()].filter((s) => s.scope === top && (s.kind === "net" || s.kind === "variable"));
  own.sort((a, b) => (isClockish(b.name) - isClockish(a.name)) || (a.width - b.width));
  state.pinned = own.slice(0, 6).map((s) => s.id);
}

// -------------------------------------------------------------------- layout

/// What an instance shows inside its box: its own signals that are not ports.
function innerSignals(instance) {
  const portSignals = new Set(instance.ports.map((p) => p.signal));
  return state.graph.signals.filter((s) => s.instance === instance.id && !portSignals.has(s.id)
    && s.kind !== "parameter" && !s.name.startsWith("$"));
}

function boxText(instance) {
  const rows = innerSignals(instance);
  const shown = state.expandedRegs.has(instance.id) ? rows : rows.slice(0, REG_ROWS);
  const processes = state.processesByInstance.get(instance.id) || [];
  return { rows, shown, processes };
}

function chipLabel(process) {
  if (process.kind === "always") {
    if (process.trigger === "implicit") return "always @*";
    if (process.sensitivity && process.sensitivity.length) {
      return "always @(" + process.sensitivity.map((s) => (s.edge === "any" ? "" : s.edge + " ") + localName(s.expression)).join(", ") + ")";
    }
    return "always";
  }
  if (process.kind === "assign") return "assign " + (process.writes[0] ? localName(process.writes[0]) : "");
  if (process.kind === "gate" || process.kind === "udp") return process.primitive || process.kind;
  return process.kind;
}

function chipRows(processes, width) {
  // Lay chips out left to right, wrapping, and remember where each went.
  const placed = [];
  let x = 10, y = 0;
  for (const process of processes) {
    const w = Math.min(chipLabel(process).length * 6 + 16, width - 20);
    if (x + w > width - 10 && x > 10) { x = 10; y += 22; }
    placed.push({ process, x, y, w });
    x += w + 6;
  }
  return { placed, height: processes.length ? y + 22 + 6 : 0 };
}

function measure(instance) {
  const { rows, shown } = boxText(instance);
  const title = instance.name + " : " + instance.module;
  const inputs = instance.ports.filter((p) => p.direction === "input");
  const outputs = instance.ports.filter((p) => p.direction !== "input");
  const portText = (ports) => Math.max(0, ...ports.map((p) => p.name.length + 8)) * 6.4;
  const regText = Math.max(0, ...shown.map((s) => s.name.length + Math.ceil((s.width || 1) / 4) + 4)) * 6.8;
  const width = Math.max(150, title.length * 7.4 + 40, portText(inputs) + portText(outputs) + 30, regText + 30);
  const folded = state.folded.has(instance.id);
  const regHeight = folded ? 0 : shown.length * ROW + (rows.length > REG_ROWS ? ROW : 0) + (rows.length ? 6 : 0);
  return { width, regHeight, folded, rows, shown };
}

/// One instance as an ELK node. `sizes` is what a first pass made of each box:
/// given it, every port is pinned below the box's header and contents, which
/// ELK will not do for a box that has children of its own.
function buildElk(instance, sizes) {
  const m = measure(instance);
  const processes = m.folded ? [] : (state.processesByInstance.get(instance.id) || []);
  const chips = chipRows(processes, m.width);
  const inputs = instance.ports.filter((p) => p.direction === "input");
  const outputs = instance.ports.filter((p) => p.direction === "output");
  const inouts = instance.ports.filter((p) => p.direction === "inout");
  const top = HEAD + m.regHeight + chips.height;
  const portsHeight = Math.max(1, inputs.length, outputs.length) * 16 + 12;
  const labelRoom = (ports) => Math.max(0, ...ports.map((p) => (p.name.length + 3 + Math.min(8, Math.ceil((p.width || 1) / 4))) * 6.2));
  const left = 16 + labelRoom(inputs);
  const right = 16 + labelRoom(outputs);
  const size = sizes && sizes.get(instance.id);
  const minimum = size ? [size.width, size.height] : [m.width, top + portsHeight + (inouts.length ? 18 : 0)];
  const fixed = (port) => {
    if (!size) return {};
    const lane = (list) => list.indexOf(port);
    const y = top + 12 + lane(port.direction === "input" ? inputs : outputs) * 16;
    if (port.direction === "input") return { x: -PORT / 2, y };
    if (port.direction === "output") return { x: size.width - PORT / 2, y };
    return { x: left + lane(inouts) * 28, y: size.height - PORT / 2 };
  };
  const node = {
    id: instance.id,
    layoutOptions: {
      "elk.padding": `[top=${top + portsHeight / 2},left=${left},bottom=${inouts.length ? 30 : 18},right=${right}]`,
      "elk.portConstraints": size ? "FIXED_POS" : "FIXED_SIDE",
      "elk.nodeSize.constraints": "MINIMUM_SIZE",
      "elk.nodeSize.minimum": `(${minimum[0]}, ${minimum[1]})`,
      "elk.spacing.portPort": "16",
    },
    ports: instance.ports.map((port) => ({
      id: instance.id + "::" + port.name,
      width: PORT, height: PORT,
      ...fixed(port),
      layoutOptions: { "elk.port.side": port.direction === "input" ? "WEST" : port.direction === "output" ? "EAST" : "SOUTH" },
    })),
    children: [],
    edges: [],
    meta: { instance, m, chips, top },
  };
  if (!m.folded) {
    for (const childId of instance.children) node.children.push(buildElk(state.instances.get(childId), sizes));
    node.edges = wiresIn(instance);
  }
  if (node.children.length === 0) {
    node.width = minimum[0];
    node.height = minimum[1];
  }
  return node;
}

/// The wires drawn inside `instance`: every signal of it that two or more of
/// its children's ports — or one of its own ports and a child's — are bound to.
function wiresIn(instance) {
  const ends = new Map(); // parent signal -> [{ port, drives, width, label }]
  const add = (signal, end) => { if (!ends.has(signal)) ends.set(signal, []); ends.get(signal).push(end); };
  for (const port of instance.ports) {
    add(canonical(port.signal), { port: instance.id + "::" + port.name, drives: port.direction !== "output", width: port.width || 1 });
  }
  const children = new Set(instance.children);
  for (const connection of state.graph.connections) {
    if (!children.has(connection.instance)) continue;
    for (const segment of connection.segments) {
      if (segment.kind !== "signal") continue;
      const storage = canonical(segment.signal);
      const whole = state.graphSignals.get(segment.signal);
      const sliced = whole && whole.width && segment.width !== whole.width;
      add(storage, {
        port: connection.instance + "::" + connection.port,
        drives: connection.direction !== "input",
        width: segment.width,
        label: sliced ? `[${segment.msb}:${segment.lsb}]` : null,
      });
    }
  }
  const edges = [];
  let n = 0;
  for (const [signal, list] of ends) {
    if (list.length < 2) continue;
    const sources = list.filter((e) => e.drives);
    const sinks = list.filter((e) => !e.drives);
    const from = sources.length ? sources : [list[0]];
    const to = sources.length ? sinks.concat(sources.slice(1)) : list.slice(1);
    for (const sink of to) {
      if (sink.port === from[0].port) continue;
      edges.push({
        id: `${instance.id}##${n++}`,
        sources: [from[0].port], targets: [sink.port],
        meta: { signal, width: Math.max(sink.width, from[0].width), label: sink.label || from[0].label },
      });
    }
  }
  return edges;
}

/// The store entry a signal ID shares, for an aliased port.
function canonical(id) {
  const signal = state.graphSignals.get(id);
  return signal && signal.storage ? signal.storage : id;
}

async function relayout(fit) {
  const first = await elk.layout(elkGraph(null));
  const sizes = new Map();
  const collect = (node) => { sizes.set(node.id, { width: node.width, height: node.height }); (node.children || []).forEach(collect); };
  collect(first.children[0]);
  state.layout = await elk.layout(elkGraph(sizes));
  render();
  if (fit) fitView();
}

function elkGraph(sizes) {
  return {
    id: "root",
    layoutOptions: {
      "elk.algorithm": "layered",
      "elk.direction": "RIGHT",
      "elk.hierarchyHandling": "INCLUDE_CHILDREN",
      "elk.json.edgeCoords": "CONTAINER",
      "elk.edgeRouting": "ORTHOGONAL",
      "elk.layered.spacing.nodeNodeBetweenLayers": "48",
      "elk.spacing.nodeNode": "28",
      "elk.spacing.edgeNode": "14",
      "elk.layered.nodePlacement.strategy": "BRANDES_KOEPF",
    },
    children: [buildElk(state.instances.get(state.graph.top), sizes)],
  };
}

// -------------------------------------------------------------------- render

function el(name, attrs = {}, parent) {
  const node = document.createElementNS(SVG, name);
  for (const [key, value] of Object.entries(attrs)) node.setAttribute(key, value);
  if (parent) parent.append(node);
  return node;
}

function bind(id, element, kind) {
  const key = canonical(id);
  if (!state.bindings.has(key)) state.bindings.set(key, []);
  state.bindings.get(key).push({ el: element, kind, id });
}

function render() {
  const viewport = $("#viewport");
  viewport.innerHTML = "";
  state.bindings = new Map();
  const node = state.layout.children[0];
  renderNode(node, viewport, 0);
  applyView();
  paintValues(false);
  paintSelection();
}

function renderNode(node, parent, d) {
  const { instance, m, chips, top } = node.meta;
  const g = el("g", { class: "instance", transform: `translate(${node.x},${node.y})`, "data-id": instance.id }, parent);
  el("rect", { class: `instance-body depth-${d === 0 ? 0 : d % 2 ? "odd" : "even"}`, width: node.width, height: node.height, rx: 8 }, g);
  if (d > 0) el("path", { class: "instance-head", d: `M0,8 a8,8 0 0 1 8,-8 h${node.width - 16} a8,8 0 0 1 8,8 v${HEAD - 8} h${-node.width} z` }, g);

  const title = el("text", { class: "instance-title", x: 10, y: 17 }, g);
  const hasChildren = instance.children.length > 0;
  if (hasChildren) {
    const chevron = el("tspan", { class: "chevron" }, title);
    chevron.textContent = m.folded ? "▶ " : "▼ ";
    chevron.addEventListener("click", (event) => { event.stopPropagation(); toggleFold(instance.id); });
  }
  const name = el("tspan", {}, title); name.textContent = instance.name;
  const module = el("tspan", { class: "module" }, title); module.textContent = "  " + instance.module;
  title.addEventListener("click", (event) => { event.stopPropagation(); select({ kind: "instance", id: instance.id }); });
  title.addEventListener("dblclick", (event) => { event.stopPropagation(); if (hasChildren) toggleFold(instance.id); });
  if (m.folded && hasChildren) {
    const note = el("text", { class: "collapsed-note", x: 10, y: HEAD + 14 }, g);
    note.textContent = `${countBelow(instance.id)} instances inside`;
  }

  // Registers and nets of this instance's own.
  let y = HEAD + 4;
  if (!m.folded) {
    for (const signal of m.shown) {
      const row = el("g", { class: "reg-row", "data-signal": signal.id, transform: `translate(0,${y})` }, g);
      el("rect", { class: "reg-bg", x: 4, y: 0, width: node.width - 8, height: ROW, rx: 3 }, row);
      const label = el("text", { class: "reg-name" + (signal.kind === "net" ? " net" : ""), x: 10, y: 12 }, row);
      label.textContent = signal.kind === "memory" ? `${signal.name}[]` : signal.name;
      if (signal.kind !== "memory" && signal.kind !== "event") {
        const value = el("text", { class: "reg-value", x: node.width - 10, y: 12 }, row);
        bind(signal.id, value, "text");
      }
      wireSignal(row, signal.id);
      y += ROW;
    }
    if (m.rows.length > REG_ROWS) {
      const more = el("text", { class: "more", x: 10, y: y + 11 }, g);
      const hidden = m.rows.length - REG_ROWS;
      more.textContent = state.expandedRegs.has(instance.id) ? "show fewer" : `+ ${hidden} more`;
      more.addEventListener("click", (event) => {
        event.stopPropagation();
        state.expandedRegs.has(instance.id) ? state.expandedRegs.delete(instance.id) : state.expandedRegs.add(instance.id);
        relayout(false);
      });
    }
    // One chip per process.
    const chipTop = HEAD + m.regHeight + 4;
    for (const { process, x, y: cy, w } of chips.placed) {
      const chip = el("g", { class: `chip ${process.kind}`, transform: `translate(${x},${chipTop + cy})` }, g);
      el("rect", { width: w, height: 18 }, chip);
      const text = el("text", { x: 8, y: 13 }, chip);
      const label = chipLabel(process);
      text.textContent = label.length * 6 + 16 > w ? label.slice(0, Math.floor((w - 16) / 6) - 1) + "…" : label;
      chip.addEventListener("mouseenter", () => highlightProcess(process, true));
      chip.addEventListener("mouseleave", () => highlightProcess(process, false));
      chip.addEventListener("click", (event) => { event.stopPropagation(); select({ kind: "process", id: process.id }); });
    }
  }

  // Ports, with their names inside the box and their values beside them.
  for (const port of node.ports || []) {
    const name = port.id.split("::")[1];
    const info = instance.ports.find((p) => p.name === name);
    const pg = el("g", { class: `port ${info.direction}`, transform: `translate(${port.x},${port.y})` }, g);
    el("rect", { width: PORT, height: PORT, rx: 2 }, pg);
    const west = port.x < node.width / 2 && info.direction === "input";
    const south = info.direction === "inout";
    const label = el("text", { class: "port-label", x: south ? 4 : west ? PORT + 5 : -5, y: south ? -6 : PORT - 0.5,
      "text-anchor": west || south ? "start" : "end" }, pg);
    const nameSpan = el("tspan", {}, label); nameSpan.textContent = name + " ";
    const valueSpan = el("tspan", { class: "port-value" }, label);
    bind(info.signal, valueSpan, "text");
    wireSignal(pg, info.signal);
  }

  for (const child of node.children || []) renderNode(child, g, d + 1);

  for (const edge of node.edges || []) {
    for (const section of edge.sections || []) {
      const points = [section.startPoint, ...(section.bendPoints || []), section.endPoint];
      const path = el("path", {
        class: "wire" + (edge.meta.width > 1 ? " bus" : ""),
        d: "M" + points.map((p) => `${p.x},${p.y}`).join(" L"),
        "data-signal": edge.meta.signal,
      }, g);
      bind(edge.meta.signal, path, edge.meta.width > 1 ? "bus" : "wire");
      wireSignal(path, edge.meta.signal);
      if (edge.meta.width > 1 || edge.meta.label) {
        const mid = points[Math.floor(points.length / 2)];
        const label = el("text", { class: "wire-label", x: mid.x + 4, y: mid.y - 4 }, g);
        label.textContent = edge.meta.label || `${edge.meta.width}`;
      }
    }
  }
  return g;
}

function countBelow(id) {
  const instance = state.instances.get(id);
  return instance.children.reduce((sum, child) => sum + 1 + countBelow(child), 0);
}

function wireSignal(element, id) {
  element.addEventListener("click", (event) => { event.stopPropagation(); select({ kind: "signal", id }); });
  element.addEventListener("dblclick", (event) => { event.stopPropagation(); pin(id); });
}

/// Puts every value on screen for `state.shownTime`; a value that moved since
/// the last paint flashes.
function paintValues(flash) {
  for (const [key, elements] of state.bindings) {
    const bits = state.values[key] ?? state.values[elements[0].id];
    for (const { el: element, kind } of elements) {
      if (kind === "text") {
        const text = formatValue(bits);
        if (flash && element.textContent !== "" && element.textContent !== text) {
          element.classList.remove("flash"); void element.getBBox; element.classList.add("flash");
        }
        element.textContent = text;
        element.setAttribute("class", element.getAttribute("class").replace(/\b(b0|b1|bx|bz|vec)\b/g, "").trim() + " " + bitClass(bits));
      } else {
        const base = kind === "bus" ? "wire bus" : "wire";
        let cls = base;
        if (kind === "wire" && typeof bits === "string") cls += " v" + bits;
        if (kind === "bus" && typeof bits === "string") cls += /x/.test(bits) ? " vx" : /^z+$/.test(bits) ? " vz" : "";
        if (state.selected && state.selected.kind === "signal" && canonical(state.selected.id) === key) cls += " selected";
        element.setAttribute("class", cls);
      }
    }
  }
}

function highlightProcess(process, on) {
  const reads = new Set(process.reads.map(canonical));
  const writes = new Set(process.writes.map(canonical));
  for (const row of document.querySelectorAll(".reg-row, .port")) {
    const id = row.querySelector("tspan.port-value") ? null : row.dataset.signal;
    const signal = canonical(row.dataset.signal || "");
    row.classList.toggle("reads", on && reads.has(signal));
    row.classList.toggle("writes", on && writes.has(signal));
  }
}

// -------------------------------------------------------------- pan and zoom

function applyView() {
  const { x, y, k } = state.view;
  $("#viewport").setAttribute("transform", `translate(${x},${y}) scale(${k})`);
}

function fitView() {
  const node = state.layout.children[0];
  const box = $("#canvas").getBoundingClientRect();
  const k = Math.min(1.6, Math.min((box.width - 40) / node.width, (box.height - 50) / node.height));
  state.view = { k, x: (box.width - node.width * k) / 2, y: Math.max(16, (box.height - node.height * k) / 2) };
  applyView();
}

function setupPanZoom() {
  const svg = $("#canvas");
  let drag = null;
  svg.addEventListener("mousedown", (event) => { drag = { x: event.clientX, y: event.clientY, vx: state.view.x, vy: state.view.y }; svg.classList.add("panning"); });
  window.addEventListener("mousemove", (event) => {
    if (!drag) return;
    state.view.x = drag.vx + event.clientX - drag.x;
    state.view.y = drag.vy + event.clientY - drag.y;
    applyView();
  });
  window.addEventListener("mouseup", () => { drag = null; svg.classList.remove("panning"); });
  svg.addEventListener("wheel", (event) => {
    event.preventDefault();
    const box = svg.getBoundingClientRect();
    const px = event.clientX - box.left, py = event.clientY - box.top;
    const factor = Math.exp(-event.deltaY * 0.0015);
    const k = Math.max(0.1, Math.min(4, state.view.k * factor));
    state.view.x = px - (px - state.view.x) * (k / state.view.k);
    state.view.y = py - (py - state.view.y) * (k / state.view.k);
    state.view.k = k;
    applyView();
  }, { passive: false });
  svg.addEventListener("click", () => select(null));
}

// ----------------------------------------------------------------- selection

function select(selection) {
  state.selected = selection;
  paintSelection();
  paintValues(false);
  renderDetails();
  if (selection && selection.kind === "instance") showSource(selection.id);
}

function paintSelection() {
  for (const row of document.querySelectorAll(".reg-row")) {
    row.classList.toggle("selected", !!state.selected && state.selected.kind === "signal" && canonical(row.dataset.signal) === canonical(state.selected.id));
  }
  for (const g of document.querySelectorAll(".instance")) {
    g.classList.toggle("selected", !!state.selected && state.selected.kind === "instance" && g.dataset.id === state.selected.id);
  }
}

function pill(text, cls, onclick) {
  const span = document.createElement("span");
  span.className = "pill " + (cls || "");
  span.textContent = text;
  if (onclick) span.addEventListener("click", onclick);
  return span;
}

function renderDetails() {
  const pane = $("#tab-details");
  pane.innerHTML = "";
  const selection = state.selected;
  if (!selection) { pane.innerHTML = '<div class="empty">Select a signal, a port or a process.</div>'; return; }
  const box = document.createElement("div");
  box.className = "detail";
  pane.append(box);
  switchTab("details");

  if (selection.kind === "signal") {
    const id = selection.id;
    const info = state.signals.get(id) || state.signals.get(canonical(id));
    const graphSignal = state.graphSignals.get(id);
    const bits = state.values[canonical(id)] ?? state.values[id];
    box.innerHTML = `<h2>${localName(id)}</h2><div class="sub">${id}</div>`;
    const big = document.createElement("div");
    big.className = "bigvalue " + bitClass(bits);
    big.textContent = typeof bits === "string" && bits.length > 1 ? `${bits.length}'h${formatValue(bits)}` : formatValue(bits);
    box.append(big);
    const rows = [
      ["at", formatTime(state.shownTime)],
      ["kind", (graphSignal && graphSignal.port ? graphSignal.port + " port, " : "") + (info ? info.kind : graphSignal ? graphSignal.kind : "?")],
      ["width", info ? `${info.width}${info.range ? ` [${info.range[0]}:${info.range[1]}]` : ""}${info.signed ? " signed" : ""}` : "–"],
    ];
    if (typeof bits === "string" && bits.length > 1) {
      rows.push(["binary", bits], ["decimal", formatValue(bits, "dec")]);
      if (info && info.signed) rows.push(["signed", signedDecimal(bits)]);
    }
    if (graphSignal && graphSignal.storage && graphSignal.storage !== id) rows.push(["same net as", graphSignal.storage]);
    box.append(table(rows));
    const actions = document.createElement("div");
    actions.className = "actions";
    const pinButton = document.createElement("button");
    pinButton.textContent = state.pinned.includes(id) ? "Unpin from waveforms" : "Pin to waveforms";
    pinButton.onclick = () => { pin(id); renderDetails(); };
    actions.append(pinButton);
    if (info && info.width === 1) {
      const edgeButton = document.createElement("button");
      edgeButton.textContent = "Run to next posedge";
      edgeButton.onclick = () => run({ kind: "edge", signal: id, edge: "posedge" });
      actions.append(edgeButton);
    }
    box.append(actions);
    const key = canonical(id);
    const writers = state.graph.processes.filter((p) => p.writes.map(canonical).includes(key));
    const readers = state.graph.processes.filter((p) => p.reads.map(canonical).includes(key));
    if (writers.length) { box.append(heading("Driven by")); writers.forEach((p) => box.append(pill(processName(p), "write", () => select({ kind: "process", id: p.id })))); }
    if (readers.length) { box.append(heading("Read by")); readers.forEach((p) => box.append(pill(processName(p), "read", () => select({ kind: "process", id: p.id })))); }
  } else if (selection.kind === "instance") {
    const instance = state.instances.get(selection.id);
    box.innerHTML = `<h2>${instance.name}</h2><div class="sub">${instance.id} · module ${instance.module}</div>`;
    if (instance.parameters.length) {
      box.append(heading("Parameters"));
      box.append(table(instance.parameters.map((p) => [p.name, p.value.text ?? p.value.decimal ?? formatValue(p.value.bits)])));
    }
    box.append(heading("Ports"));
    box.append(table(instance.ports.map((p) => [`${p.direction} ${p.name}`, formatValue(state.values[canonical(p.signal)])])));
  } else if (selection.kind === "process") {
    const process = state.graph.processes.find((p) => p.id === selection.id);
    box.innerHTML = `<h2>${chipLabel(process)}</h2><div class="sub">${process.id}</div>`;
    if (process.writes.length) { box.append(heading("Writes")); process.writes.forEach((s) => box.append(pill(s, "write", () => select({ kind: "signal", id: s })))); }
    if (process.reads.length) { box.append(heading("Reads")); process.reads.forEach((s) => box.append(pill(s, "read", () => select({ kind: "signal", id: s })))); }
    showSource(process.instance);
    switchTab("details");
  }
}

function processName(process) { return `${process.instance.split(".").pop()}: ${chipLabel(process)}`; }

function heading(text) { const h = document.createElement("h3"); h.textContent = text; return h; }

function table(rows) {
  const t = document.createElement("table");
  for (const [key, value] of rows) {
    const tr = t.insertRow();
    tr.insertCell().textContent = key;
    const cell = tr.insertCell();
    cell.className = "mono";
    cell.textContent = value;
  }
  return t;
}

function switchTab(name) {
  for (const button of document.querySelectorAll(".tabs button")) button.classList.toggle("active", button.dataset.tab === name);
  for (const tab of document.querySelectorAll(".tab")) tab.classList.toggle("active", tab.id === "tab-" + name);
}

// -------------------------------------------------------------------- source

const sourceCache = new Map();

async function showSource(instanceId) {
  const instance = state.instances.get(instanceId);
  const module = state.graph.modules.find((m) => m.name === instance.module);
  const pane = $("#tab-source");
  if (!module || !module.source) { pane.innerHTML = '<div class="empty">No source location for this module.</div>'; return; }
  const { file, line } = module.source;
  if (!sourceCache.has(file)) {
    try { sourceCache.set(file, await api("/api/source?path=" + encodeURIComponent(file))); }
    catch { pane.innerHTML = '<div class="empty">Source not available.</div>'; return; }
  }
  const text = sourceCache.get(file);
  pane.innerHTML = "";
  const head = document.createElement("div");
  head.className = "source-head";
  head.textContent = `${file}:${line}`;
  const pre = document.createElement("pre");
  pre.className = "source";
  text.split("\n").forEach((lineText, index) => {
    const span = document.createElement("span");
    span.className = "ln" + (index + 1 === line ? " here" : "");
    span.innerHTML = highlight(lineText) || " ";
    pre.append(span);
  });
  pane.append(head, pre);
  const here = pre.querySelector(".here");
  if (here) setTimeout(() => { pane.scrollTop = here.offsetTop - 60; }, 0);
}

const KEYWORDS = new Set("module endmodule input output inout wire reg integer always initial begin end if else case casez casex endcase default assign parameter localparam posedge negedge for while repeat forever function endfunction task endtask generate endgenerate genvar signed real time event fork join wait".split(" "));

function highlight(line) {
  const escape = (s) => s.replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;");
  const comment = line.indexOf("//");
  const code = comment >= 0 ? line.slice(0, comment) : line;
  const rest = comment >= 0 ? `<span class="com">${escape(line.slice(comment))}</span>` : "";
  const html = escape(code).replace(/("[^"]*")|(\$[a-zA-Z_]\w*)|(\b\d+'[sS]?[bBoOdDhH][0-9a-fA-FxXzZ_?]+|\b\d+\b)|(\b[a-z_]\w*\b)/g,
    (match, string, system, number, word) => {
      if (string) return `<span class="str">${string}</span>`;
      if (system) return `<span class="sys">${system}</span>`;
      if (number) return `<span class="num">${number}</span>`;
      if (KEYWORDS.has(word)) return `<span class="kw">${word}</span>`;
      return match;
    });
  return html + rest;
}

// ------------------------------------------------------------------- running

async function refreshValues(time) {
  const query = time === undefined ? "" : "?t=" + time;
  const result = await api("/api/values" + query);
  state.values = result.values;
  state.shownTime = result.time;
}

async function refreshTraces() {
  if (state.pinned.length === 0) { state.traces = {}; drawWaves(); return; }
  const result = await api("/api/traces?ids=" + encodeURIComponent(state.pinned.join(",")));
  state.traces = result.traces;
  state.now = result.now;
  drawWaves();
}

async function refreshConsole() {
  const result = await api("/api/console?from=" + state.consoleLength);
  if (result.text) {
    const pre = $("#console");
    pre.textContent += result.text;
    pre.scrollTop = pre.scrollHeight;
  }
  state.consoleLength = result.length;
}

function updateClock(s) {
  state.now = s.time;
  $("#time").textContent = formatTime(s.time);
  const status = $("#status");
  status.className = s.finished ? "finished" : "";
  status.textContent = (s.finished ? "finished · " : "") + `${s.steps.toLocaleString()} timesteps` + (s.truncated ? " · history full" : "");
}

async function run(request) {
  if (state.busy) return;
  setBusy(true);
  try {
    const result = await api("/api/run", request);
    if (result.error) { toast(result.error, true); return; }
    updateClock(result.state);
    const reason = result.stop.reason;
    if (result.stop.error) toast(result.stop.error, true);
    else if (reason === "breakpoint") toast("Stopped at the breakpoint");
    else if (reason === "finished") toast("The design called $finish");
    else if (reason === "quiescent") toast("Nothing left scheduled");
    else if (reason === "step_limit" && request.kind !== "steps" && request.kind !== "step") toast("Stopped after 2,000,000 timesteps — run again to go on");
    leaveHistory(false);
    await refreshValues();
    paintValues(true);
    renderDetails();
    await refreshTraces();
    if (result.state.console_length !== state.consoleLength) await refreshConsole();
  } catch (error) {
    toast(String(error.message || error), true);
  } finally {
    setBusy(false);
  }
}

function setBusy(busy) {
  state.busy = busy;
  for (const button of document.querySelectorAll("#toolbar button")) button.disabled = busy;
  $("#status").textContent = busy ? "running…" : $("#status").textContent;
}

let toastTimer = null;
function toast(text, error) {
  const t = $("#toast");
  t.textContent = text;
  t.style.color = error ? "var(--red)" : "var(--fg)";
  t.hidden = false;
  clearTimeout(toastTimer);
  toastTimer = setTimeout(() => { t.hidden = true; }, 3200);
}

async function toggleFold(id) {
  state.folded.has(id) ? state.folded.delete(id) : state.folded.add(id);
  await relayout(false);
}

function pin(id) {
  const index = state.pinned.indexOf(id);
  if (index >= 0) state.pinned.splice(index, 1); else state.pinned.push(id);
  refreshTraces();
}

// ------------------------------------------------------------ history mode

async function showTime(time) {
  time = Math.max(0, Math.min(state.now, Math.round(time)));
  await refreshValues(time);
  paintValues(true);
  renderDetails();
  $("#history-badge").hidden = time === state.now;
  drawWaves();
}

function leaveHistory(repaint = true) {
  $("#history-badge").hidden = true;
  if (repaint) showTime(state.now);
}

// --------------------------------------------------------------- waveforms

const LABEL = 190;
const LANE = 26;

function waveRange() {
  return state.waveWindow || [0, Math.max(1, state.now)];
}

function drawWaves() {
  if (!state.design) return;
  const canvas = $("#waves-canvas");
  const ratio = window.devicePixelRatio || 1;
  const rect = canvas.getBoundingClientRect();
  canvas.width = rect.width * ratio;
  canvas.height = rect.height * ratio;
  const ctx = canvas.getContext("2d");
  ctx.scale(ratio, ratio);
  ctx.clearRect(0, 0, rect.width, rect.height);
  const [t0, t1] = waveRange();
  const plot = rect.width - LABEL - 12;
  const xOf = (t) => LABEL + ((t - t0) / (t1 - t0)) * plot;
  const css = getComputedStyle(document.documentElement);
  const color = (name) => css.getPropertyValue(name).trim();

  // Axis.
  ctx.font = "10px " + color("--mono");
  ctx.fillStyle = color("--muted");
  ctx.strokeStyle = "#232739";
  const ticks = niceTicks(t0, t1, Math.max(2, Math.floor(plot / 110)));
  for (const t of ticks) {
    const x = xOf(t);
    ctx.beginPath(); ctx.moveTo(x, 16); ctx.lineTo(x, rect.height); ctx.stroke();
    ctx.fillText(formatTime(t), x + 3, 11);
  }

  state.pinned.forEach((id, lane) => {
    const top = 22 + lane * LANE;
    const mid = top + LANE / 2;
    const trace = state.traces[id] || [];
    const atCursor = state.values[canonical(id)] ?? state.values[id];
    ctx.fillStyle = state.selected && state.selected.id === id ? color("--blue") : color("--fg");
    ctx.font = "11px " + color("--mono");
    ctx.fillText(ellipsize(localName(id), 16), 10, mid + 4);
    ctx.fillStyle = color(typeof atCursor === "string" && atCursor.length === 1 ? { "1": "--green", x: "--red", z: "--teal" }[atCursor] || "--dim" : "--dim");
    ctx.fillText(ellipsize(formatValue(atCursor), 9), 128, mid + 4);
    ctx.strokeStyle = "#232739";
    ctx.beginPath(); ctx.moveTo(0, top + LANE); ctx.lineTo(rect.width, top + LANE); ctx.stroke();

    const width = (state.signals.get(id) || { width: 1 }).width;
    for (let i = 0; i < trace.length; i++) {
      const [start, value] = trace[i];
      const end = i + 1 < trace.length ? trace[i + 1][0] : state.now;
      if (end < t0 || start > t1) continue;
      const x0 = Math.max(LABEL, xOf(start)), x1 = Math.min(LABEL + plot, xOf(end));
      if (width === 1 && typeof value === "string") {
        const hi = top + 5, lo = top + LANE - 5;
        if (value === "x" || value === "z") {
          ctx.fillStyle = value === "x" ? "rgba(247,118,142,.25)" : "rgba(115,218,202,.15)";
          ctx.fillRect(x0, hi, x1 - x0, lo - hi);
          ctx.strokeStyle = value === "x" ? color("--red") : color("--teal");
          ctx.beginPath(); ctx.moveTo(x0, mid); ctx.lineTo(x1, mid); ctx.stroke();
        } else {
          const y = value === "1" ? hi : lo;
          ctx.strokeStyle = value === "1" ? color("--green") : "#6b7394";
          ctx.lineWidth = 1.4;
          ctx.beginPath();
          const prev = i > 0 ? trace[i - 1][1] : value;
          if (i > 0 && start >= t0) ctx.moveTo(x0, prev === "1" ? hi : prev === "0" ? lo : mid); else ctx.moveTo(x0, y);
          ctx.lineTo(x0, y); ctx.lineTo(x1, y); ctx.stroke();
          ctx.lineWidth = 1;
        }
      } else {
        const unknown = typeof value === "string" && /[xz]/.test(value);
        ctx.strokeStyle = unknown ? color("--red") : "#5868a3";
        ctx.fillStyle = unknown ? "rgba(247,118,142,.12)" : "rgba(88,104,163,.18)";
        const s = 3, hi = top + 5, lo = top + LANE - 5;
        ctx.beginPath();
        ctx.moveTo(x0, mid); ctx.lineTo(Math.min(x0 + s, x1), hi); ctx.lineTo(Math.max(x1 - s, x0), hi);
        ctx.lineTo(x1, mid); ctx.lineTo(Math.max(x1 - s, x0), lo); ctx.lineTo(Math.min(x0 + s, x1), lo); ctx.closePath();
        ctx.fill(); ctx.stroke();
        const label = formatValue(value);
        ctx.font = "10px " + color("--mono");
        if (ctx.measureText(label).width + 10 < x1 - x0) {
          ctx.fillStyle = color("--fg");
          ctx.fillText(label, x0 + 6, mid + 3.5);
        }
      }
    }
  });

  // Cursor and now.
  const now = xOf(state.now);
  ctx.strokeStyle = "rgba(125,207,255,.35)";
  ctx.setLineDash([3, 3]);
  ctx.beginPath(); ctx.moveTo(now, 14); ctx.lineTo(now, rect.height); ctx.stroke();
  ctx.setLineDash([]);
  if (state.shownTime !== state.now) {
    const cursor = xOf(state.shownTime);
    ctx.strokeStyle = color("--orange");
    ctx.beginPath(); ctx.moveTo(cursor, 14); ctx.lineTo(cursor, rect.height); ctx.stroke();
  }
  if (state.pinned.length === 0) {
    ctx.fillStyle = color("--muted");
    ctx.font = "12px " + color("--sans");
    ctx.fillText("Double-click any signal in the diagram to pin it here.", LABEL, 50);
  }
}

function ellipsize(text, n) { return text.length > n ? text.slice(0, n - 1) + "…" : text; }

function niceTicks(t0, t1, count) {
  const span = t1 - t0;
  const raw = span / count;
  const power = Math.pow(10, Math.floor(Math.log10(raw)));
  const step = Math.max(1, [1, 2, 5, 10].map((m) => m * power).find((s) => s >= raw) || raw);
  const ticks = [];
  for (let t = Math.ceil(t0 / step) * step; t <= t1; t += step) ticks.push(t);
  return ticks;
}

function setupWaves() {
  const canvas = $("#waves-canvas");
  canvas.addEventListener("click", (event) => {
    const rect = canvas.getBoundingClientRect();
    const x = event.clientX - rect.left;
    if (x < LABEL) {
      const lane = Math.floor((event.clientY - rect.top - 22) / LANE);
      if (state.pinned[lane]) select({ kind: "signal", id: state.pinned[lane] });
      return;
    }
    const [t0, t1] = waveRange();
    showTime(t0 + ((x - LABEL) / (rect.width - LABEL - 12)) * (t1 - t0));
  });
  canvas.addEventListener("dblclick", (event) => {
    const rect = canvas.getBoundingClientRect();
    if (event.clientX - rect.left >= LABEL) return;
    const lane = Math.floor((event.clientY - rect.top - 22) / LANE);
    if (state.pinned[lane]) pin(state.pinned[lane]);
  });
  canvas.addEventListener("wheel", (event) => {
    event.preventDefault();
    const rect = canvas.getBoundingClientRect();
    let [t0, t1] = waveRange();
    const span = t1 - t0;
    if (event.ctrlKey || event.metaKey) {
      const at = t0 + ((event.clientX - rect.left - LABEL) / (rect.width - LABEL - 12)) * span;
      const factor = Math.exp(event.deltaY * 0.002);
      const next = Math.max(2, Math.min(Math.max(1, state.now), span * factor));
      t0 = at - (at - t0) * (next / span);
      t1 = t0 + next;
    } else {
      const shift = (event.deltaX || event.deltaY) * span * 0.002;
      t0 += shift; t1 += shift;
    }
    if (t0 < 0) { t1 -= t0; t0 = 0; }
    if (t1 > state.now) { t0 -= t1 - state.now; t1 = state.now; t0 = Math.max(0, t0); }
    state.waveWindow = t1 - t0 >= state.now ? null : [t0, t1];
    drawWaves();
  }, { passive: false });
  $("#waves-fit").onclick = () => { state.waveWindow = null; drawWaves(); };
  new ResizeObserver(drawWaves).observe(canvas);
}

// ---------------------------------------------------------------- controls

function setupControls() {
  $("#step").onclick = () => run({ kind: "step" });
  $("#step-n").onclick = () => run({ kind: "steps", count: Math.max(1, parseInt($("#step-count").value, 10) || 1) });
  $("#run-edge").onclick = () => run({ kind: "edge", signal: $("#edge-signal").value, edge: $("#edge-kind").value });
  $("#run-break").onclick = () => { const expression = $("#break-expr").value.trim(); if (expression) run({ kind: "breakpoint", expression }); };
  $("#break-expr").addEventListener("keydown", (event) => { if (event.key === "Enter") $("#run-break").click(); });
  $("#run-all").onclick = () => run({ kind: "run" });
  $("#reset").onclick = async () => {
    if (state.busy) return;
    setBusy(true);
    try { const design = await api("/api/reset", {}); await load(design); toast("Back at time zero"); }
    finally { setBusy(false); }
  };
  $("#fit").onclick = fitView;
  $("#unfold").onclick = () => { state.folded.clear(); relayout(true); };
  $("#fold").onclick = () => {
    for (const instance of state.graph.instances) if (instance.parent && instance.children.length) state.folded.add(instance.id);
    relayout(true);
  };
  $("#back-to-now").onclick = (event) => { event.preventDefault(); leaveHistory(); };
  for (const button of document.querySelectorAll(".tabs button")) button.onclick = () => switchTab(button.dataset.tab);
  window.addEventListener("keydown", (event) => {
    if (event.target.matches("input, select, textarea")) return;
    if (event.key === "ArrowRight" || event.key === " ") { event.preventDefault(); run({ kind: "step" }); }
    else if (event.key === "e") $("#run-edge").click();
    else if (event.key === "f") fitView();
    else if (event.key === "r") $("#reset").click();
    else if (event.key === "Escape") select(null);
  });
}

setupPanZoom();
setupWaves();
setupControls();
load().catch((error) => toast("Could not load the design: " + error.message, true));
