const minNodeSpacing = 30;
let allNodeNames = [];
let searchedNode = null;
let actualHeight, actualWidth, currentHeight;
let currentData, hierarchyRoot, isFirstRender;
let zoom, container;

function render() {
    initLayout();
    initZoom();
    createContainer();
    createUI();
    prepareState();
    update();
    setTimeout(centerOnRoot, 100);
}

function initLayout() {
    actualHeight = (data.height || 1000) - 4;
    actualWidth = calculateResponsiveWidth();
    setSvgAttributes();
    window.addEventListener("resize", () => 
        svg.attr("width", calculateResponsiveWidth()));
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
    container = svg.append("g").attr("class", "tree-container");
}

function createUI() {
    createButton(20, 20, "Center root", "#007acc", "#005a9e", centerOnRoot);
    createButton(20, 60, "Expand All", "#28a745", "#1e7e34", expandAll);
    createButton(20, 100, "Fold All", "#dc3545", "#c82333", foldAll);
    createSearchBar();
}

function createButton(x, y, label, fill, stroke, callback) {
    const g = svg.append("g")
        .attr("transform", `translate(${x}, ${y})`)
        .style("cursor", "pointer")
        .on("click", callback);
    
    g.append("rect")
        .attr("width", 100).attr("height", 30).attr("rx", 5)
        .style("fill", fill).style("stroke", stroke)
        .style("stroke-width", 1);
    
    g.append("text")
        .attr("x", 50).attr("y", 20).attr("text-anchor", "middle")
        .style("fill", "white").style("font-size", "12px")
        .style("font-weight", "bold").style("pointer-events", "none")
        .text(label);
}

function centerOnRoot() {
    if (!hierarchyRoot) return;
    const t = d3.zoomIdentity
        .translate(actualWidth / 2 - (hierarchyRoot.y || 0),
                   actualHeight / 2 - (hierarchyRoot.x || 0))
        .scale(1);
    svg.transition().duration(750).call(zoom.transform, t);
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
        .attr("list", "tree-genre-datalist")
        .style("width", "290px").style("height", "26px")
        .style("margin", "2px 5px").style("border", "none")
        .style("outline", "none").style("font-size", "12px")
        .style("background", "transparent")
        .on("keypress", function(event) {
            if (event.key === "Enter" && this.value.trim() && 
                allNodeNames.includes(this.value.trim())) {
                focusOnNode(this.value.trim());
                this.value = "";
            }
        });
    
    const dl = fo.append("xhtml:datalist").attr("id", "tree-genre-datalist");
    allNodeNames.slice().sort()
        .forEach(name => dl.append("xhtml:option").attr("value", name));
}

function focusOnNode(name) {
    if (!hierarchyRoot) return;
    
    searchedNode = name;
    const target = findNodeByName(hierarchyRoot, name);
    if (!target) return;
    
    expandPathToNode(hierarchyRoot, name);
    update();
    
    setTimeout(() => {
        const t = d3.zoomIdentity
            .translate(actualWidth / 2 - (target.y || 0),
                      actualHeight / 2 - (target.x || 0))
            .scale(1);
        svg.transition().duration(750).call(zoom.transform, t);
    }, 100);
}

function expandPathToNode(root, targetName) {
    function expand(node) {
        if (node.data.name === targetName) return true;
        
        let found = false;
        [node._children, node.children].forEach(arr => {
            if (arr) found = arr.some(expand) || found;
        });
        
        if (found && node._children) {
            node.children = node._children;
            node._children = null;
        }
        
        return found;
    }
    
    expand(root);
}

function prepareState() {
    currentData = JSON.parse(JSON.stringify(data.tree));
    hierarchyRoot = null;
    isFirstRender = true;
    currentHeight = actualHeight;
    
    allNodeNames = [];
    (function collect(node) {
        allNodeNames.push(node.name);
        node.children?.forEach(collect);
    })(currentData);
}

function update() {
    container.selectAll("*").remove();
    
    if (isFirstRender) {
        hierarchyRoot = d3.hierarchy(currentData);
        collapseToLevel(hierarchyRoot, 0);
        sortBySize(hierarchyRoot);
        isFirstRender = false;
    }
    
    const root = hierarchyRoot;
    currentHeight = calculateHeight(root);
    
    d3.tree().size([currentHeight, actualWidth])(root);
    
    root.descendants().forEach(d => {
        d.x = d.x - currentHeight / 2 + actualHeight / 2;
        d.y = d.y;
    });
    
    const sizeScale = d3.scaleLinear()
        .domain([0, d3.max(root.descendants()
            .map(d => getEffSize(d))) || 1])
        .range([4, 15]);
    
    const wLookup = {};
    if (data.weights?.key) {
        data.weights.key.forEach((k, i) => 
            wLookup[k] = data.weights.weight[i]);
    }
    
    const weights = root.links()
        .map(l => wLookup[`${l.target.data.name}->${l.source.data.name}`])
        .filter(w => w != null);
    
    const wScales = weights.length > 0 ? {
        width: d3.scaleLinear().domain(d3.extent(weights)).range([0.5, 4]),
        color: d3.scaleLinear().domain(d3.extent(weights))
            .range(["#e0e0e0", "#000000"])
    } : {};
    
    renderLinks(root, wLookup, wScales);
    renderNodes(root, sizeScale);
}

function collapseToLevel(node, depth = 0) {
    if (depth >= 1 && node.children) {
        node._children = node.children;
        node.children = null;
    }
    [node.children, node._children].forEach(arr => 
        arr?.forEach(c => collapseToLevel(c, depth + 1)));
}

function sortBySize(node) {
    [node.children, node._children].forEach(arr => {
        arr?.sort((a, b) => totalSize(b) - totalSize(a));
        arr?.forEach(sortBySize);
    });
}

function findNodeByName(root, name) {
    if (root.data.name === name) return root;
    for (const arr of [root.children, root._children]) {
        if (arr) {
            for (const c of arr) {
                const f = findNodeByName(c, name);
                if (f) return f;
            }
        }
    }
    return null;
}

function hasExpandable(name) {
    if (!name || !hierarchyRoot) return false;
    const n = findNodeByName(hierarchyRoot, name);
    return n && (n.children || n._children);
}

function isCollapsed(name) {
    if (!name || !hierarchyRoot) return false;
    const n = findNodeByName(hierarchyRoot, name);
    return n && n._children && !n.children;
}

function totalSize(node) {
    return (node.data.size || 0) + 
           (node.children?.reduce((s, c) => s + totalSize(c), 0) || 0) +
           (node._children?.reduce((s, c) => s + totalSize(c), 0) || 0);
}

function getEffSize(node) {
    const n = findNodeByName(hierarchyRoot, node.data.name);
    return n && isCollapsed(node.data.name) ? totalSize(n) : node.data.size || 0;
}

function toggle(d) {
    if (!d.data) return;
    const n = findNodeByName(hierarchyRoot, d.data.name);
    if (!n) return;
    
    if (n.children) {
        n._children = n.children;
        n.children = null;
    } else if (n._children) {
        n.children = n._children;
        n._children = null;
    }
    
    sortBySize(hierarchyRoot);
    update();
}

function expandAll() {
    if (!hierarchyRoot) return;
    
    (function expand(n) {
        if (n._children) {
            n.children = n._children;
            n._children = null;
        }
        n.children?.forEach(expand);
    })(hierarchyRoot);
    
    sortBySize(hierarchyRoot);
    update();
    
    setTimeout(() => {
        const t = d3.zoomIdentity
            .translate(actualWidth / 2 - (hierarchyRoot.y || 0),
                      actualHeight / 2 - (hierarchyRoot.x || 0))
            .scale(0.7);
        svg.transition().duration(750).call(zoom.transform, t);
    }, 100);
}

function foldAll() {
    if (!hierarchyRoot) return;
    
    (function collapse(n) {
        if (n.children) {
            n._children = n.children;
            n.children = null;
        }
        n._children?.forEach(collapse);
    })(hierarchyRoot);
    
    sortBySize(hierarchyRoot);
    update();
    setTimeout(centerOnRoot, 100);
}

function calculateHeight(root) {
    const levels = {};
    root.descendants().forEach(n => 
        (levels[n.depth] = levels[n.depth] || []).push(n));
    const max = Math.max(...Object.values(levels).map(a => a.length));
    return Math.max(600, max * minNodeSpacing);
}

function renderLinks(root, wLookup, wScales) {
    const links = container.selectAll(".link")
        .data(root.links()).enter().append("path")
        .attr("class", "link")
        .attr("d", d3.linkHorizontal().x(d => d.y).y(d => d.x))
        .style("fill", "none")
        .style("stroke", d => {
            const w = wLookup[`${d.target.data.name}->${d.source.data.name}`];
            return w != null && wScales.color ? wScales.color(w) : "#ccc";
        })
        .style("stroke-width", d => {
            const w = wLookup[`${d.target.data.name}->${d.source.data.name}`];
            return w != null && wScales.width ? wScales.width(w) : 2;
        });
    
    links.append("title").text(d => {
        const w = wLookup[`${d.target.data.name}->${d.source.data.name}`];
        const base = `${d.target.data.name} → ${d.source.data.name}`;
        return w != null ? `${base}\nWeight: ${w.toFixed(4)}` : base;
    });
}

function renderNodes(root, sizeScale) {
    const g = container.selectAll(".node")
        .data(root.descendants()).enter().append("g")
        .attr("class", "node")
        .attr("transform", d => `translate(${d.y}, ${d.x})`)
        .style("cursor", d => hasExpandable(d.data.name) ? "pointer" : "default");
    
    g.append("circle")
        .attr("r", d => sizeScale(getEffSize(d)))
        .style("fill", d => 
            searchedNode && d.data.name === searchedNode ? "#28a745" : 
            d.data.fill || "#69b3a2")
        .style("stroke", d => 
            searchedNode && d.data.name === searchedNode ? "#1e7e34" :
            hasExpandable(d.data.name) ? "#000" : "none")
        .style("stroke-width", d => 
            searchedNode && d.data.name === searchedNode ? 3 :
            hasExpandable(d.data.name) ? 2 : 1)
        .on("click", (e, d) => {
            e.stopPropagation();
            const n = findNodeByName(hierarchyRoot, d.data.name);
            if (n && (n._children || n.children)) toggle(d);
        })
        .on("mouseover", function(e, d) {
            if (hasExpandable(d.data.name)) 
                d3.select(this).style("stroke-width", 4);
        })
        .on("mouseout", function(e, d) {
            const w = searchedNode && d.data.name === searchedNode ? 3 :
                     hasExpandable(d.data.name) ? 2 : 1;
            d3.select(this).style("stroke-width", w);
        });
    
    g.filter(d => hasExpandable(d.data.name))
        .append("text")
        .attr("text-anchor", "middle").attr("dy", 4)
        .style("font-size", "10px").style("font-weight", "bold")
        .style("fill", "white").style("pointer-events", "none")
        .text(d => isCollapsed(d.data.name) ? "+" : "−");
    
    g.append("text")
        .attr("dy", 3)
        .attr("x", d => hasExpandable(d.data.name) ? -18 : 18)
        .style("text-anchor", d => hasExpandable(d.data.name) ? "end" : "start")
        .style("font-size", "12pt")
        .style("font-weight", d => hasExpandable(d.data.name) ? "bold" : "normal")
        .style("text-decoration", d => hasExpandable(d.data.name) ? "underline" : "none")
        .style("fill", d => hasExpandable(d.data.name) ? "#0066cc" : "#333")
        .style("cursor", d => hasExpandable(d.data.name) ? "pointer" : "default")
        .text(d => d.data.name)
        .on("click", (e, d) => {
            e.stopPropagation();
            const n = findNodeByName(hierarchyRoot, d.data.name);
            if (n && (n._children || n.children)) toggle(d);
        })
        .on("mouseover", function(e, d) {
            if (hasExpandable(d.data.name)) 
                d3.select(this).style("fill", "#0044aa");
        })
        .on("mouseout", function(e, d) {
            d3.select(this).style("fill", 
                hasExpandable(d.data.name) ? "#0066cc" : "#333");
        });
    
    g.append("title").text(d => {
        const eff = getEffSize(d);
        const orig = d.data.size || 0;
        const name = d.data.name;
        
        if (hasExpandable(name)) {
            const action = isCollapsed(name) ? "expand" : "collapse";
            return isCollapsed(name) && eff > orig ? 
                   `${name} (Click to ${action}) - Aggregated size: ${eff} (own: ${orig})` :
                   `${name} (Click to ${action}) - Size: ${eff}`;
        }
        
        return `${name} - Size: ${eff}`;
    });
}

render();