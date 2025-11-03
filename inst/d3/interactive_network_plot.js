const minNodeSpacing = 30;
let allNodeNames = []; // For search functionality
let searchedNode = null; // Currently searched/highlighted node

function render() {
    setLayoutSizes();
    setSvgAttributes();
    addResizeHandler();
    initZoomBehavior();
    createContainer();
    createControlButtons();
    prepareState();
    update();
    createSearchBar(); // Add search bar after state is prepared
    setTimeout(centerOnRoot, 100);
}

function setLayoutSizes() {
    actualHeight = (data.height || 1000) - 4;
    actualWidth = getResponsiveWidth();
}

function getResponsiveWidth() {
    const node = svg.node();
    const style = window.getComputedStyle(node);
    if (style.width && style.width !== "auto") {
        const w = parseFloat(style.width);
        if (w > 0) return w - 4;
    }
    return window.innerWidth * 0.8;
}

function setSvgAttributes() {
    svg.attr("width", actualWidth)
       .attr("height", actualHeight)
       .style("background-color", "#fafafa")
       .style("overflow", "hidden")
       .style("cursor", "grab")
       .style("display", "block");
}

function addResizeHandler() {
    window.addEventListener("resize", function() {
        svg.attr("width", getResponsiveWidth());
    });
}

function initZoomBehavior() {
    zoom = d3.zoom()
        .scaleExtent([0.1, 3])
        .on("zoom", function(event) {
            container.attr("transform", event.transform);
            svg.style("cursor",
                event.sourceEvent &&
                event.sourceEvent.type === "mousemove"
                ? "grabbing" : "grab");
        });
    svg.call(zoom);
}

function createContainer() {
    container = svg.append("g").attr("class", "tree-container");
}

function createControlButtons() {
    createButton(20, 20, "Center root", "#007acc", "#005a9e", centerOnRoot);
    createButton(20, 60, "Expand All", "#28a745", "#1e7e34", expandAll);
    createButton(20, 100, "Fold All", "#dc3545", "#c82333", foldAll);
}

function createButton(x, y, label, fill, stroke, cb) {
    const g = svg.append("g").attr("transform", `translate(${x}, ${y})`)
        .style("cursor", "pointer");
    g.append("rect")
        .attr("width", 100).attr("height", 30).attr("rx", 5)
        .style("fill", fill).style("stroke", stroke)
        .style("stroke-width", 1);
    g.append("text").attr("x", 50).attr("y", 20)
        .attr("text-anchor", "middle").style("fill", "white")
        .style("font-size", "12px").style("font-weight", "bold")
        .style("pointer-events", "none").text(label);
    g.on("click", cb);
}

function centerOnRoot() {
    if (!hierarchyRoot) return;
    const rx = hierarchyRoot.x || 0;
    const ry = hierarchyRoot.y || 0;
    const cx = actualWidth / 2;
    const cy = actualHeight / 2;
    const t = d3.zoomIdentity.translate(cx - ry, cy - rx).scale(1);
    svg.transition().duration(750).call(zoom.transform, t);
}

function createSearchBar() {
    const searchContainer = svg.append("g")
        .attr("transform", `translate(${actualWidth / 2 - 150}, 20)`);
    
    // Background rect
    searchContainer.append("rect")
        .attr("width", 300).attr("height", 30).attr("rx", 5)
        .style("fill", "white").style("stroke", "#ccc")
        .style("stroke-width", 1);
    
    // Create foreignObject for HTML input
    const fo = searchContainer.append("foreignObject")
        .attr("width", 300).attr("height", 30);
    
    const input = fo.append("xhtml:input")
        .attr("type", "text")
        .attr("placeholder", "Search genre...")
        .attr("list", "tree-genre-datalist")
        .style("width", "290px")
        .style("height", "26px")
        .style("margin", "2px 5px")
        .style("border", "none")
        .style("outline", "none")
        .style("font-size", "12px")
        .style("background", "transparent")
        .on("keypress", function(event) {
            if (event.key === "Enter") {
                const value = this.value.trim();
                if (value && allNodeNames.includes(value)) {
                    focusOnNode(value);
                    this.value = "";
                }
            }
        });
    
    // Create datalist for autocomplete
    const datalist = fo.append("xhtml:datalist")
        .attr("id", "tree-genre-datalist");
    
    allNodeNames.sort().forEach(name => {
        datalist.append("xhtml:option").attr("value", name);
    });
}

function focusOnNode(nodeName) {
    if (!hierarchyRoot) return;
    
    searchedNode = nodeName;
    
    // Find and expand path to node
    const targetNode = findNodeByName(hierarchyRoot, nodeName);
    if (!targetNode) return;
    
    // Expand all ancestors
    expandPathToNode(hierarchyRoot, nodeName);
    
    // Update visualization
    update();
    
    // Center on the node with default zoom
    setTimeout(() => {
        const nx = targetNode.x || 0;
        const ny = targetNode.y || 0;
        const cx = actualWidth / 2;
        const cy = actualHeight / 2;
        const t = d3.zoomIdentity.translate(cx - ny, cy - nx).scale(1);
        svg.transition().duration(750).call(zoom.transform, t);
    }, 100);
}

function expandPathToNode(root, targetName) {
    function expand(node) {
        if (node.data.name === targetName) return true;
        
        let foundInChild = false;
        if (node._children) {
            for (let child of node._children) {
                if (expand(child)) {
                    foundInChild = true;
                }
            }
        }
        if (node.children) {
            for (let child of node.children) {
                if (expand(child)) {
                    foundInChild = true;
                }
            }
        }
        
        if (foundInChild && node._children) {
            node.children = node._children;
            node._children = null;
        }
        
        return foundInChild;
    }
    
    expand(root);
}

function prepareState() {
    currentData = JSON.parse(JSON.stringify(data.tree));
    hierarchyRoot = null;
    isFirstRender = true;
    currentHeight = actualHeight;
    
    // Collect all node names for search
    allNodeNames = [];
    function collectNames(node) {
        allNodeNames.push(node.name);
        if (node.children) node.children.forEach(collectNames);
    }
    collectNames(currentData);
}

function collapseHierarchyToFirstLevel(node, depth = 0) {
    if (depth >= 1 && node.children) {
        node._children = node.children;
        node.children = null;
    }
    if (node.children) node.children.forEach(c =>
        collapseHierarchyToFirstLevel(c, depth + 1));
    if (node._children) node._children.forEach(c =>
        collapseHierarchyToFirstLevel(c, depth + 1));
}

function sortChildrenBySize(node) {
    if (node.children) {
        node.children.sort((a, b) => calculateTotalSize(b)
            - calculateTotalSize(a));
        node.children.forEach(sortChildrenBySize);
    }
    if (node._children) {
        node._children.sort((a, b) => calculateTotalSize(b)
            - calculateTotalSize(a));
        node._children.forEach(sortChildrenBySize);
    }
}

function findNodeByName(root, name) {
    if (root.data.name === name) return root;
    if (root.children) {
        for (let c of root.children) {
            const f = findNodeByName(c, name);
            if (f) return f;
        }
    }
    if (root._children) {
        for (let c of root._children) {
            const f = findNodeByName(c, name);
            if (f) return f;
        }
    }
    return null;
}

function hasExpandableChildren(nodeName) {
    if (!nodeName || !hierarchyRoot) return false;
    const n = findNodeByName(hierarchyRoot, nodeName);
    return n && (n.children || n._children);
}

function isCollapsed(nodeName) {
    if (!nodeName || !hierarchyRoot) return false;
    const n = findNodeByName(hierarchyRoot, nodeName);
    return n && n._children && !n.children;
}

function calculateTotalSize(node) {
    let total = node.data.size || 0;
    if (node.children) for (let c of node.children) total += calculateTotalSize(c);
    if (node._children) for (let c of node._children) total += calculateTotalSize(c);
    return total;
}

function getEffectiveSize(displayNode, nodeName) {
    const p = findNodeByName(hierarchyRoot, nodeName);
    if (p && isCollapsed(nodeName)) return calculateTotalSize(p);
    return displayNode.data.size || 0;
}

function toggle(d) {
    if (!d.data) return;
    const p = findNodeByName(hierarchyRoot, d.data.name);
    if (!p) return;
    if (p.children) { p._children = p.children; p.children = null; }
    else if (p._children) { p.children = p._children; p._children = null; }
    sortChildrenBySize(hierarchyRoot);
    update();
}

function expandAll() {
    if (!hierarchyRoot) return;
    (function expand(node) {
        if (node._children) { node.children = node._children; node._children = null; }
        if (node.children) node.children.forEach(expand);
    }(hierarchyRoot));
    sortChildrenBySize(hierarchyRoot);
    update();
    setTimeout(function() {
        if (!hierarchyRoot) return;
        const rx = hierarchyRoot.x || 0;
        const ry = hierarchyRoot.y || 0;
        const cx = actualWidth / 2;
        const cy = actualHeight / 2;
        const t = d3.zoomIdentity.translate(cx - ry, cy - rx).scale(0.7);
        svg.transition().duration(750).call(zoom.transform, t);
    }, 100);
}

function foldAll() {
    if (!hierarchyRoot) return;
    (function collapse(node) {
        if (node.children) { node._children = node.children; node.children = null; }
        if (node._children) node._children.forEach(collapse);
    }(hierarchyRoot));
    sortChildrenBySize(hierarchyRoot);
    update();
    setTimeout(centerOnRoot, 100);
}

function prepareWeightsLookup() {
    const out = {};
    if (data.weights && Array.isArray(data.weights.key)) {
        for (let i = 0; i < data.weights.key.length; i++) {
            out[data.weights.key[i]] = data.weights.weight[i];
        }
    }
    return out;
}

function getEdgeWeight(link, wLookup) {
    if (!wLookup || Object.keys(wLookup).length === 0) return null;
    const key = link.target.data.name + "->" + link.source.data.name;
    return wLookup[key] || null;
}

function createScales(root) {
    currentHeight = calculateRequiredHeight(root);
    const allEff = root.descendants().map(d => getEffectiveSize(d, d.data.name));
    const maxVal = d3.max(allEff) || 1;
    const sizeScale = d3.scaleLinear().domain([0, maxVal]).range([4, 15]);
    return sizeScale;
}

function calculateRequiredHeight(root) {
    const nodes = root.descendants();
    const levels = {};
    nodes.forEach(n => {
        const d = n.depth;
        (levels[d] = levels[d] || []).push(n);
    });
    const maxAt = Math.max(...Object.values(levels).map(a => a.length));
    return Math.max(600, maxAt * minNodeSpacing);
}

function renderLinks(root, wLookup, strokeScales) {
    const links = container.selectAll(".link").data(root.links()).enter().append("path")
        .attr("class", "link")
        .attr("d", d3.linkHorizontal().x(d => d.y).y(d => d.x))
        .style("fill", "none")
        .style("stroke", d => {
            const w = getEdgeWeight(d, wLookup);
            return (w !== null && strokeScales && strokeScales.color)
                ? strokeScales.color(w) : "#ccc";
        })
        .style("stroke-width", d => {
            const w = getEdgeWeight(d, wLookup);
            return (w !== null && strokeScales && strokeScales.width)
                ? strokeScales.width(w) : 2;
        });
    links.append("title").text(d => {
        const w = getEdgeWeight(d, wLookup);
        if (w !== null) return `${d.target.data.name} → ${d.source.data.name}\nWeight: ${w.toFixed(4)}`;
        return `${d.target.data.name} → ${d.source.data.name}`;
    });
}

function renderNodes(root, sizeScale) {
    const nodes = container.selectAll(".node").data(root.descendants()).enter().append("g")
        .attr("class", "node")
        .attr("transform", d => `translate(${d.y}, ${d.x})`)
        .style("cursor", d => (d.data && hasExpandableChildren(d.data.name)) ? "pointer" : "default");

    nodes.append("circle")
        .attr("r", d => sizeScale(getEffectiveSize(d, d.data.name)))
        .style("fill", d => {
            // Highlight searched node in green
            if (searchedNode && d.data.name === searchedNode) return "#28a745";
            return d.data.fill || "#69b3a2";
        })
        .style("stroke", d => {
            if (searchedNode && d.data.name === searchedNode) return "#1e7e34";
            return (d.data && hasExpandableChildren(d.data.name)) ? "#000" : "none";
        })
        .style("stroke-width", d => {
            if (searchedNode && d.data.name === searchedNode) return 3;
            return (d.data && hasExpandableChildren(d.data.name)) ? 2 : 1;
        })
        .on("click", function(event, d) {
            event.stopPropagation();
            const p = findNodeByName(hierarchyRoot, d.data.name);
            if (p && (p._children || p.children)) toggle(d);
        })
        .on("mouseover", function(event, d) {
            if (d.data && hasExpandableChildren(d.data.name)) d3.select(this).style("stroke-width", 4);
        })
        .on("mouseout", function(event, d) {
            if (searchedNode && d.data.name === searchedNode) {
                d3.select(this).style("stroke-width", 3);
            } else {
                d3.select(this).style("stroke-width", (d.data && hasExpandableChildren(d.data.name)) ? 2 : 1);
            }
        });

    nodes.filter(d => d.data && hasExpandableChildren(d.data.name))
        .append("text").attr("text-anchor", "middle").attr("dy", 4)
        .style("font-size", "10px").style("font-weight", "bold")
        .style("fill", "white").style("pointer-events", "none")
        .text(d => isCollapsed(d.data.name) ? "+" : "−");

    nodes.append("text")
        .attr("dy", 3)
        .attr("x", d => hasExpandableChildren(d.data.name) ? -18 : 18)
        .style("text-anchor", d => hasExpandableChildren(d.data.name) ? "end" : "start")
        .style("font-size", "12pt")
        .style("font-weight", d => hasExpandableChildren(d.data.name) ? "bold" : "normal")
        .style("text-decoration", d => hasExpandableChildren(d.data.name) ? "underline" : "none")
        .style("fill", d => hasExpandableChildren(d.data.name) ? "#0066cc" : "#333")
        .style("cursor", d => hasExpandableChildren(d.data.name) ? "pointer" : "default")
        .text(d => d.data.name)
        .on("click", function(event, d) {
            event.stopPropagation();
            const p = findNodeByName(hierarchyRoot, d.data.name);
            if (p && (p._children || p.children)) toggle(d);
        })
        .on("mouseover", function(event, d) {
            if (hasExpandableChildren(d.data.name)) d3.select(this).style("fill", "#0044aa");
        })
        .on("mouseout", function(event, d) {
            d3.select(this).style("fill", hasExpandableChildren(d.data.name) ? "#0066cc" : "#333");
        });

    nodes.append("title").text(d => {
        const eff = getEffectiveSize(d, d.data.name);
        const orig = d.data.size || 0;
        if (hasExpandableChildren(d.data.name)) {
            const action = isCollapsed(d.data.name) ? "expand" : "collapse";
            if (isCollapsed(d.data.name) && eff > orig) {
                return `${d.data.name} (Click to ${action}) - Aggregated size: ${eff} (own: ${orig})`;
            }
            return `${d.data.name} (Click to ${action}) - Size: ${eff}`;
        }
        return `${d.data.name} - Size: ${eff}`;
    });
}

function computeStrokeScales(root, wLookup) {
    const allW = root.links().map(l => getEdgeWeight(l, wLookup)).filter(w => w !== null);
    if (allW.length === 0) return {};
    const minW = d3.min(allW);
    const maxW = d3.max(allW);
    return {
        width: d3.scaleLinear().domain([minW, maxW]).range([0.5, 4]),
        color: d3.scaleLinear().domain([minW, maxW]).range(["#e0e0e0", "#000000"])
    };
}

function update() {
    container.selectAll("*").remove();
    if (isFirstRender) {
        hierarchyRoot = d3.hierarchy(currentData);
        collapseHierarchyToFirstLevel(hierarchyRoot, 0);
        sortChildrenBySize(hierarchyRoot);
        isFirstRender = false;
    }
    const root = hierarchyRoot;
    currentHeight = calculateRequiredHeight(root);
    const treeLayout = d3.tree().size([currentHeight, actualWidth]);
    treeLayout(root);
    const startX = 0;
    const startY = actualHeight / 2;
    root.descendants().forEach(d => {
        d.x = d.x - currentHeight / 2 + startY;
        d.y = d.y + startX;
    });
    const sizeScale = createScales(root);
    const wLookup = prepareWeightsLookup();
    const strokeScales = computeStrokeScales(root, wLookup);
    renderLinks(root, wLookup, strokeScales);
    renderNodes(root, sizeScale);
}

let actualHeight, actualWidth, currentHeight;
let currentData, hierarchyRoot, isFirstRender;
let zoom, container;
render();