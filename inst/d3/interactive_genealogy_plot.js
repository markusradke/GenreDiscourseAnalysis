let actualHeight, actualWidth;
let currentFocus;
let zoom, container;
let allNodesMap, allEdges;
let rootName;
let focusHistory = [];
let edgeThreshold = 0;
let treeEdges = new Set();

function render() {
    initLayout();
    initZoom();
    createContainer();
    prepareState();
    createUI();
    update();
    setTimeout(resetPosition, 100);
}

function initLayout() {
    actualHeight = (data.height || 800) - 4;
    actualWidth = calculateResponsiveWidth();
    setSvgAttributes();
    window.addEventListener("resize", () => {
        actualWidth = calculateResponsiveWidth();
        svg.attr("width", actualWidth);
    });
}

function calculateResponsiveWidth() {
    const style = window.getComputedStyle(svg.node());
    const width = parseFloat(style.width);
    return style.width && style.width !== "auto" && width > 0 ? 
           width - 4 : window.innerWidth * 0.8;
}

function setSvgAttributes() {
    svg.attr("width", actualWidth)
        .attr("height", actualHeight)
        .style("background-color", "#fafafa")
        .style("overflow", "hidden")
        .style("cursor", "grab")
        .style("display", "block");
}

function initZoom() {
    zoom = d3.zoom()
        .scaleExtent([0.1, 3])
        .on("zoom", event => {
            container.attr("transform", event.transform);
            svg.style("cursor", event.sourceEvent?.type === "mousemove" ? 
                     "grabbing" : "grab");
        });
    svg.call(zoom);
}

function createContainer() {
    container = svg.append("g").attr("class", "genealogy-container");
}

function createUI() {
    createButton(20, 20, "Focus Root", "#007acc", "#005a9e", focusRoot);
    createButton(20, 60, "Reset Position", "#28a745", "#1e7e34", 
                 resetPosition);
    createButton(20, 100, "Back", "#6c757d", "#5a6268", goBack);
    createSearchBar();
    createEdgeSlider();
}

function createButton(x, y, label, fill, stroke, callback) {
    const g = svg.append("g")
        .attr("transform", `translate(${x}, ${y})`)
        .style("cursor", "pointer")
        .on("click", callback);
    
    g.append("rect")
        .attr("width", 150).attr("height", 30).attr("rx", 5)
        .style("fill", fill).style("stroke", stroke)
        .style("stroke-width", 1);
    
    g.append("text")
        .attr("x", 75).attr("y", 20).attr("text-anchor", "middle")
        .style("fill", "white").style("font-size", "12px")
        .style("font-weight", "bold").style("pointer-events", "none")
        .text(label);
}

function resetPosition() {
    const t = d3.zoomIdentity
        .translate(actualWidth / 2, 120).scale(1);
    svg.transition().duration(750).call(zoom.transform, t);
}

function focusRoot() {
    if (rootName) changeFocus(rootName);
}

function goBack() {
    if (focusHistory.length < 2) return;
    focusHistory.pop();
    const prev = focusHistory.pop();
    if (prev) changeFocus(prev, false);
}

function createSearchBar() {
    const g = svg.append("g")
        .attr("transform", `translate(${actualWidth / 2 - 150}, 20)`);
    
    g.append("rect")
        .attr("width", 300).attr("height", 30).attr("rx", 5)
        .style("fill", "white").style("stroke", "#ccc")
        .style("stroke-width", 1);
    
    const fo = g.append("foreignObject")
        .attr("width", 300).attr("height", 30);
    
    fo.append("xhtml:input")
        .attr("type", "text")
        .attr("placeholder", "Search genre...")
        .attr("list", "genre-datalist")
        .style("width", "290px").style("height", "26px")
        .style("margin", "2px 5px").style("border", "none")
        .style("outline", "none").style("font-size", "12px")
        .style("background", "transparent")
        .on("keypress", function(event) {
            if (event.key === "Enter" && this.value.trim() && 
                allNodesMap[this.value.trim()]) {
                changeFocus(this.value.trim());
                this.value = "";
            }
        });
    
    const dl = fo.append("xhtml:datalist").attr("id", "genre-datalist");
    Object.keys(allNodesMap).sort()
        .forEach(name => dl.append("xhtml:option").attr("value", name));
}

function createEdgeSlider() {
    const config = { x: actualWidth - 80, y: 150, h: 200 };
    const g = svg.append("g")
        .attr("transform", `translate(${config.x}, ${config.y})`);
    
    ["Edge", "Threshold"].forEach((text, i) => {
        g.append("text")
            .attr("x", 0).attr("y", -35 + i * 12)
            .attr("text-anchor", "middle")
            .style("font-size", "11px").style("font-weight", "bold")
            .text(text);
    });
    
    g.append("line")
        .attr("x1", 0).attr("x2", 0).attr("y1", 0).attr("y2", config.h)
        .style("stroke", "#ccc").style("stroke-width", 4);
    
    const scale = d3.scaleLinear().domain([0, 1]).range([0, config.h]);
    const handle = g.append("circle")
        .attr("cx", 0).attr("cy", scale(edgeThreshold)).attr("r", 8)
        .style("fill", "#007acc").style("stroke", "#005a9e")
        .style("stroke-width", 2).style("cursor", "pointer");
    
    const valueText = g.append("text")
        .attr("x", 20).attr("y", scale(edgeThreshold) + 4)
        .style("font-size", "11px").text(edgeThreshold.toFixed(2));
    
    handle.call(d3.drag().on("drag", event => {
        const y = Math.max(0, Math.min(config.h, event.y));
        edgeThreshold = scale.invert(y);
        handle.attr("cy", y);
        valueText.attr("y", y + 4).text(edgeThreshold.toFixed(2));
        changeFocus(currentFocus, false);
    }));
}

function prepareState() {
    currentFocus = data.genealogy.focus.name;
    rootName = data.root_name;
    focusHistory = [currentFocus];
    
    allNodesMap = {};
    if (data.genealogy.all_nodes?.name) {
        data.genealogy.all_nodes.name.forEach((name, i) => {
            allNodesMap[name] = {
                name, 
                size: data.genealogy.all_nodes.size[i],
                fill: data.genealogy.all_nodes.fill[i]
            };
        });
    }
    
    allEdges = [];
    if (data.genealogy.edges?.from) {
        for (let i = 0; i < data.genealogy.edges.from.length; i++) {
            allEdges.push({
                from: data.genealogy.edges.from[i],
                to: data.genealogy.edges.to[i],
                weight: data.genealogy.edges.weight?.[i] ?? null
            });
        }
    }
    
    treeEdges = new Set();
    if (data.tree_edges?.from) {
        for (let i = 0; i < data.tree_edges.from.length; i++) {
            treeEdges.add(`${data.tree_edges.from[i]}->${data.tree_edges.to[i]}`);
        }
    }
    
    [data.genealogy.parents, data.genealogy.children].forEach(arr => {
        arr?.forEach(node => {
            const key = arr === data.genealogy.parents ? 
                       `${currentFocus}->${node.name}` : 
                       `${node.name}->${currentFocus}`;
            node.isTreeNode = treeEdges.has(key);
        });
        arr?.sort((a, b) => (b.weight ?? -Infinity) - (a.weight ?? -Infinity));
    });
}

function changeFocus(newName, addHistory = true) {
    if (!allNodesMap[newName]) return;
    
    currentFocus = newName;
    if (addHistory) focusHistory.push(newName);
    
    const focus = allNodesMap[newName];
    const parents = [], children = [];
    
    allEdges.forEach(edge => {
        if (edge.from === newName) {
            const node = allNodesMap[edge.to];
            if (!node) return;
            
            const key = `${edge.from}->${edge.to}`;
            const isTree = treeEdges.has(key);
            const weight = edge.weight ?? 0;
            
            if (isTree || weight >= edgeThreshold) {
                parents.push({ 
                    ...node, 
                    weight: edge.weight, 
                    isTreeNode: isTree 
                });
            }
        } else if (edge.to === newName) {
            const node = allNodesMap[edge.from];
            if (!node) return;
            
            const key = `${edge.from}->${edge.to}`;
            const isTree = treeEdges.has(key);
            const weight = edge.weight ?? 0;
            
            if (isTree || weight >= edgeThreshold) {
                children.push({ 
                    ...node, 
                    weight: edge.weight, 
                    isTreeNode: isTree 
                });
            }
        }
    });
    
    [parents, children].forEach(arr => 
        arr.sort((a, b) => (b.weight ?? -Infinity) - (a.weight ?? -Infinity))
    );
    
    data.genealogy = { focus, parents, children };
    update();
    setTimeout(resetPosition, 100);
}

function update() {
    container.selectAll("*").remove();
    
    const gen = data.genealogy;
    const sizes = [gen.focus.size, 
                   ...(gen.parents?.map(p => p.size) || []),
                   ...(gen.children?.map(c => c.size) || [])];
    const sizeScale = d3.scaleLinear()
        .domain([0, d3.max(sizes) || 1]).range([8, 20]);
    
    const weights = [...(gen.parents || []), ...(gen.children || [])]
        .map(n => n.weight).filter(w => w != null && !isNaN(w));
    
    const weightScales = weights.length > 0 ? {
        width: d3.scaleLinear().domain(d3.extent(weights)).range([1, 5]),
        color: d3.scaleLinear().domain(d3.extent(weights))
            .range(["#e0e0e0", "#000000"])
    } : {};
    
    const cfg = { top: 120, cx: 0, px: -300, chx: 300, sp: 40 };
    const pos = {
        parents: (gen.parents || []).map((p, i) => 
            ({ ...p, x: cfg.px, y: cfg.top + i * cfg.sp })),
        children: (gen.children || []).map((c, i) => 
            ({ ...c, x: cfg.chx, y: cfg.top + i * cfg.sp })),
        focus: { ...gen.focus, x: cfg.cx, y: cfg.top }
    };
    
    [
        { nodes: pos.parents, cls: "link-parent", src: pos.focus },
        { nodes: pos.children, cls: "link-child", tgt: pos.focus }
    ].forEach(({ nodes, cls, src, tgt }) => {
        if (!nodes.length) return;
        const links = container.selectAll(`.${cls}`)
            .data(nodes).enter().append("path").attr("class", cls)
            .attr("d", d => {
                const s = src || d, t = tgt || d;
                const mx = (s.x + t.x) / 2;
                return `M ${s.x},${s.y} C ${mx},${s.y} ${mx},${t.y} ${t.x},${t.y}`;
            })
            .style("fill", "none")
            .style("stroke", d => 
                d.weight != null && weightScales.color ? 
                weightScales.color(d.weight) : "#ccc")
            .style("stroke-width", d => 
                d.weight != null && weightScales.width ? 
                weightScales.width(d.weight) : 2);
        
        links.append("title").text(d => {
            const [from, to] = cls === "link-parent" ? 
                              [gen.focus.name, d.name] : [d.name, gen.focus.name];
            return d.weight != null ? 
                   `${from} → ${to}\nWeight: ${d.weight.toFixed(4)}` : 
                   `${from} → ${to}`;
        });
    });
    
    [
        { nodes: pos.parents, type: "parent" },
        { nodes: [pos.focus], type: "focus" },
        { nodes: pos.children, type: "child" }
    ].forEach(({ nodes, type }) => drawNodes(nodes, sizeScale, type));
}

function drawNodes(nodes, sizeScale, type) {
    const g = container.selectAll(`.node-${type}`)
        .data(nodes).enter().append("g")
        .attr("class", `node node-${type}`)
        .attr("transform", d => `translate(${d.x}, ${d.y})`)
        .style("cursor", "pointer");
    
    g.append("circle")
        .attr("r", d => sizeScale(d.size))
        .style("fill", d => d.isTreeNode ? "#28a745" : d.fill || "#69b3a2")
        .style("stroke", d => 
            type === "focus" ? "#000" : 
            d.isTreeNode ? "#1e7e34" : "#555")
        .style("stroke-width", d => 
            type === "focus" ? 3 : d.isTreeNode ? 2.5 : 1.5)
        .on("click", (e, d) => { e.stopPropagation(); changeFocus(d.name); })
        .on("mouseover", function(e, d) {
            const w = parseFloat(d3.select(this).style("stroke-width"));
            d3.select(this).style("stroke-width", w + 2);
        })
        .on("mouseout", function(e, d) {
            const w = type === "focus" ? 3 : d.isTreeNode ? 2.5 : 1.5;
            d3.select(this).style("stroke-width", w);
        });
    
    g.append("text")
        .attr("dy", type === "focus" ? -25 : 3)
        .attr("x", type === "parent" ? -25 : type === "child" ? 25 : 0)
        .style("text-anchor", 
            type === "parent" ? "end" : type === "child" ? "start" : "middle")
        .style("font-size", type === "focus" ? "14pt" : "12pt")
        .style("font-weight", type === "focus" ? "bold" : "normal")
        .style("fill", type === "focus" ? "#000" : "#0066cc")
        .style("cursor", "pointer")
        .text(d => d.name)
        .on("click", (e, d) => { e.stopPropagation(); changeFocus(d.name); })
        .on("mouseover", function() {
            d3.select(this).style("fill", type === "focus" ? "#000" : "#0044aa");
        })
        .on("mouseout", function() {
            d3.select(this).style("fill", type === "focus" ? "#000" : "#0066cc");
        });
    
    g.append("title").text(d => {
        const label = type === "parent" ? "Parent genre" : 
                     type === "child" ? "Subgenre" : "Focused genre";
        const tree = d.isTreeNode ? " [TREE]" : "";
        return `${d.name}${tree} (${label}) - Size: ${d.size.toFixed(2)}`;
    });
}

render();