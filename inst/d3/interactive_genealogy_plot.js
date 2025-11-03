let actualHeight, actualWidth;
let currentFocus;
let zoom, container;
let allNodesMap, allEdges;
let rootName;
let focusHistory = []; // Track focus history for back button
let edgeThreshold = 0; // Minimum edge weight to display
let treeEdges = new Set(); // Edges that are part of the tree (always shown)

// Initialize the visualization
function render() {
    setLayoutSizes();
    setSvgAttributes();
    addResizeHandler();
    initZoomBehavior();
    createContainer();
    prepareState();
    createBasicButtons(); // Just the basic action buttons
    update();
    createSearchBar(); // After state is ready
    createEdgeThresholdSlider(); // After state is ready
    setTimeout(resetPosition, 100);
}

function setLayoutSizes() {
    actualHeight = (data.height || 800) - 4;
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
        actualWidth = getResponsiveWidth();
    });
}

function initZoomBehavior() {
    zoom = d3.zoom()
        .scaleExtent([0.1, 3])
        .on("zoom", function(event) {
            container.attr("transform", event.transform);
            svg.style("cursor",
                event.sourceEvent &&
                event.sourceEvent.type === "mousemove" ?
                "grabbing" : "grab");
        });
    svg.call(zoom);
}

function createContainer() {
    container = svg.append("g").attr("class", "genealogy-container");
}

function createBasicButtons() {
    createButton(20, 20, "Focus Root", "#007acc", "#005a9e", focusRoot);
    createButton(20, 60, "Reset Position", "#28a745", "#1e7e34", resetPosition);
    createButton(20, 100, "Back", "#6c757d", "#5a6268", goBack);
}

function createButton(x, y, label, fill, stroke, cb) {
    const g = svg.append("g").attr("transform", `translate(${x}, ${y})`)
        .style("cursor", "pointer");
    g.append("rect")
        .attr("width", 150).attr("height", 30).attr("rx", 5)
        .style("fill", fill).style("stroke", stroke)
        .style("stroke-width", 1);
    g.append("text").attr("x", 75).attr("y", 20)
        .attr("text-anchor", "middle").style("fill", "white")
        .style("font-size", "12px").style("font-weight", "bold")
        .style("pointer-events", "none").text(label);
    g.on("click", cb);
}

function resetPosition() {
    const topMargin = 120;
    const cx = actualWidth / 2;
    // Position view so top-aligned content is visible
    const t = d3.zoomIdentity.translate(cx, topMargin).scale(1);
    svg.transition().duration(750).call(zoom.transform, t);
}

function focusRoot() {
    if (!rootName) return;
    changeFocus(rootName);
}

function goBack() {
    if (focusHistory.length < 2) return; // Need at least 2 items (current + previous)
    focusHistory.pop(); // Remove current
    const previous = focusHistory.pop(); // Get previous (will be re-added by changeFocus)
    if (previous) {
        changeFocus(previous, false); // false = don't add to history
    }
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
        .attr("list", "genre-datalist")
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
                if (value && allNodesMap[value]) {
                    changeFocus(value);
                    this.value = "";
                }
            }
        });

    // Create datalist for autocomplete
    const datalist = fo.append("xhtml:datalist")
        .attr("id", "genre-datalist");

    Object.keys(allNodesMap).sort().forEach(name => {
        datalist.append("xhtml:option").attr("value", name);
    });
}

function createEdgeThresholdSlider() {
    const sliderX = actualWidth - 80;
    const sliderY = 150;
    const sliderHeight = 200;

    const sliderGroup = svg.append("g")
        .attr("transform", `translate(${sliderX}, ${sliderY})`);

    // Label
    sliderGroup.append("text")
        .attr("x", 0).attr("y", -35)
        .attr("text-anchor", "middle")
        .style("font-size", "11px")
        .style("font-weight", "bold")
        .text("Edge");

    sliderGroup.append("text")
        .attr("x", 0).attr("y", -23)
        .attr("text-anchor", "middle")
        .style("font-size", "11px")
        .style("font-weight", "bold")
        .text("Threshold");

    // Track
    sliderGroup.append("line")
        .attr("x1", 0).attr("x2", 0)
        .attr("y1", 0).attr("y2", sliderHeight)
        .style("stroke", "#ccc")
        .style("stroke-width", 4);

    // Scale for slider
    const sliderScale = d3.scaleLinear()
        .domain([0, 1])
        .range([0, sliderHeight]);

    // Draggable handle
    const handle = sliderGroup.append("circle")
        .attr("cx", 0).attr("cy", sliderScale(edgeThreshold))
        .attr("r", 8)
        .style("fill", "#007acc")
        .style("stroke", "#005a9e")
        .style("stroke-width", 2)
        .style("cursor", "pointer");

    // Value display
    const valueText = sliderGroup.append("text")
        .attr("x", 20).attr("y", sliderScale(edgeThreshold) + 4)
        .style("font-size", "11px")
        .text(edgeThreshold.toFixed(2));

    // Drag behavior
    const drag = d3.drag()
        .on("drag", function(event) {
            const newY = Math.max(0, Math.min(sliderHeight, event.y));
            const newThreshold = sliderScale.invert(newY);
            edgeThreshold = newThreshold;

            handle.attr("cy", newY);
            valueText.attr("y", newY + 4).text(newThreshold.toFixed(2));

            // Rebuild current focus with new threshold (don't add to history)
            changeFocus(currentFocus, false);
        });

    handle.call(drag);
}

function prepareState() {
    currentFocus = data.genealogy.focus.name;
    rootName = data.root_name;
    focusHistory = [currentFocus];

    // Build lookup maps for all nodes and edges
    allNodesMap = {};
    if (data.genealogy.all_nodes && data.genealogy.all_nodes.name) {
        data.genealogy.all_nodes.name.forEach((name, i) => {
            allNodesMap[name] = {
                name: name,
                size: data.genealogy.all_nodes.size[i],
                fill: data.genealogy.all_nodes.fill[i]
            };
        });
    }

    allEdges = [];
    if (data.genealogy.edges && data.genealogy.edges.from) {
        for (let i = 0; i < data.genealogy.edges.from.length; i++) {
            allEdges.push({
                from: data.genealogy.edges.from[i],
                to: data.genealogy.edges.to[i],
                weight: data.genealogy.edges.weight ? data.genealogy.edges.weight[i] : null
            });
        }
    }

    // Identify tree edges from the tree_edges data
    treeEdges = new Set();
    if (data.tree_edges && data.tree_edges.from) {
        for (let i = 0; i < data.tree_edges.from.length; i++) {
            const key = data.tree_edges.from[i] + "->" + data.tree_edges.to[i];
            treeEdges.add(key);
        }
    }

    // Mark tree nodes in initial parents and children
    if (data.genealogy.parents) {
        data.genealogy.parents.forEach(p => {
            const edgeKey = currentFocus + "->" + p.name;
            p.isTreeNode = treeEdges.has(edgeKey);
        });

        data.genealogy.parents.sort((a, b) => {
            const aWeight = a.weight !== null && !isNaN(a.weight) ? a.weight : -Infinity;
            const bWeight = b.weight !== null && !isNaN(b.weight) ? b.weight : -Infinity;
            return bWeight - aWeight;
        });
    }

    if (data.genealogy.children) {
        data.genealogy.children.forEach(c => {
            const edgeKey = c.name + "->" + currentFocus;
            c.isTreeNode = treeEdges.has(edgeKey);
        });

        data.genealogy.children.sort((a, b) => {
            const aWeight = a.weight !== null && !isNaN(a.weight) ? a.weight : -Infinity;
            const bWeight = b.weight !== null && !isNaN(b.weight) ? b.weight : -Infinity;
            return bWeight - aWeight;
        });
    }
}

function changeFocus(newFocusName, addToHistory = true) {
    if (!allNodesMap[newFocusName]) return;

    currentFocus = newFocusName;

    // Add to history
    if (addToHistory) {
        focusHistory.push(newFocusName);
    }

    // Reconstruct genealogy structure for new focus
    const focusNode = allNodesMap[newFocusName];

    // Find parents (edges where from=focus)
    const parents = [];
    allEdges.forEach(edge => {
        if (edge.from === newFocusName) {
            const parentNode = allNodesMap[edge.to];
            const edgeKey = edge.from + "->" + edge.to;
            const isTreeEdge = treeEdges.has(edgeKey);
            const weight = edge.weight !== null && !isNaN(edge.weight) ? edge.weight : 0;

            // Include if: tree edge OR weight >= threshold
            if (parentNode && (isTreeEdge || weight >= edgeThreshold)) {
                parents.push({
                    name: parentNode.name,
                    size: parentNode.size,
                    fill: parentNode.fill,
                    weight: edge.weight,
                    isTreeNode: isTreeEdge
                });
            }
        }
    });

    // Find children (edges where to=focus)
    const children = [];
    allEdges.forEach(edge => {
        if (edge.to === newFocusName) {
            const childNode = allNodesMap[edge.from];
            const edgeKey = edge.from + "->" + edge.to;
            const isTreeEdge = treeEdges.has(edgeKey);
            const weight = edge.weight !== null && !isNaN(edge.weight) ? edge.weight : 0;

            // Include if: tree edge OR weight >= threshold
            if (childNode && (isTreeEdge || weight >= edgeThreshold)) {
                children.push({
                    name: childNode.name,
                    size: childNode.size,
                    fill: childNode.fill,
                    weight: edge.weight,
                    isTreeNode: isTreeEdge
                });
            }
        }
    });

    // Sort by weight (strongest first) - handle null weights
    parents.sort((a, b) => {
        const aWeight = a.weight !== null && !isNaN(a.weight) ? a.weight : -Infinity;
        const bWeight = b.weight !== null && !isNaN(b.weight) ? b.weight : -Infinity;
        return bWeight - aWeight;
    });

    children.sort((a, b) => {
        const aWeight = a.weight !== null && !isNaN(a.weight) ? a.weight : -Infinity;
        const bWeight = b.weight !== null && !isNaN(b.weight) ? b.weight : -Infinity;
        return bWeight - aWeight;
    });

    data.genealogy.focus = focusNode;
    data.genealogy.parents = parents;
    data.genealogy.children = children;

    update();
    setTimeout(resetPosition, 100);
}

function createScales(genealogy) {
    const allSizes = [genealogy.focus.size];

    if (genealogy.parents) {
        genealogy.parents.forEach(p => allSizes.push(p.size));
    }
    if (genealogy.children) {
        genealogy.children.forEach(c => allSizes.push(c.size));
    }

    const maxSize = d3.max(allSizes) || 1;
    const sizeScale = d3.scaleLinear().domain([0, maxSize]).range([8, 20]);

    // Weight scales for edges
    const allWeights = [];
    if (genealogy.parents) {
        genealogy.parents.forEach(p => {
            if (p.weight !== null && !isNaN(p.weight)) allWeights.push(p.weight);
        });
    }
    if (genealogy.children) {
        genealogy.children.forEach(c => {
            if (c.weight !== null && !isNaN(c.weight)) allWeights.push(c.weight);
        });
    }

    let weightScales = {};
    if (allWeights.length > 0) {
        const minW = d3.min(allWeights);
        const maxW = d3.max(allWeights);
        weightScales = {
            width: d3.scaleLinear().domain([minW, maxW]).range([1, 5]),
            color: d3.scaleLinear().domain([minW, maxW]).range(["#e0e0e0", "#000000"])
        };
    }

    return {
        sizeScale,
        weightScales
    };
}

function update() {
    container.selectAll("*").remove();

    const genealogy = data.genealogy;
    const {
        sizeScale,
        weightScales
    } = createScales(genealogy);

    // Define positions
    const topMargin = 120; // Space for buttons
    const centerX = 0;
    const parentX = -300;
    const childX = 300;

    // Fixed vertical spacing to ensure proper spacing and avoid overlap
    const verticalSpacing = 40;

    // Position parents - start from top
    let parentPositions = [];
    if (genealogy.parents && genealogy.parents.length > 0) {
        parentPositions = genealogy.parents.map((p, i) => ({
            ...p,
            x: parentX,
            y: topMargin + i * verticalSpacing
        }));
    }

    // Position children - start from top
    let childPositions = [];
    if (genealogy.children && genealogy.children.length > 0) {
        childPositions = genealogy.children.map((c, i) => ({
            ...c,
            x: childX,
            y: topMargin + i * verticalSpacing
        }));
    }

    // Focus node position - align to top as well
    const focusPosition = {
        ...genealogy.focus,
        x: centerX,
        y: topMargin
    };

    // Draw links from focus to parents
    if (parentPositions.length > 0) {
        const parentLinks = container.selectAll(".link-parent")
            .data(parentPositions)
            .enter()
            .append("path")
            .attr("class", "link-parent")
            .attr("d", d => {
                return `M ${focusPosition.x},${focusPosition.y} 
                        C ${(focusPosition.x + d.x) / 2},${focusPosition.y} 
                          ${(focusPosition.x + d.x) / 2},${d.y} 
                          ${d.x},${d.y}`;
            })
            .style("fill", "none")
            .style("stroke", d => {
                if (d.weight !== null && !isNaN(d.weight) && weightScales.color) {
                    return weightScales.color(d.weight);
                }
                return "#ccc";
            })
            .style("stroke-width", d => {
                if (d.weight !== null && !isNaN(d.weight) && weightScales.width) {
                    return weightScales.width(d.weight);
                }
                return 2;
            });

        parentLinks.append("title").text(d => {
            if (d.weight !== null && !isNaN(d.weight)) {
                return `${genealogy.focus.name} → ${d.name}\nWeight: ${d.weight.toFixed(4)}`;
            }
            return `${genealogy.focus.name} → ${d.name}`;
        });
    }

    // Draw links from children to focus
    if (childPositions.length > 0) {
        const childLinks = container.selectAll(".link-child")
            .data(childPositions)
            .enter()
            .append("path")
            .attr("class", "link-child")
            .attr("d", d => {
                return `M ${d.x},${d.y} 
                        C ${(d.x + focusPosition.x) / 2},${d.y} 
                          ${(d.x + focusPosition.x) / 2},${focusPosition.y} 
                          ${focusPosition.x},${focusPosition.y}`;
            })
            .style("fill", "none")
            .style("stroke", d => {
                if (d.weight !== null && !isNaN(d.weight) && weightScales.color) {
                    return weightScales.color(d.weight);
                }
                return "#ccc";
            })
            .style("stroke-width", d => {
                if (d.weight !== null && !isNaN(d.weight) && weightScales.width) {
                    return weightScales.width(d.weight);
                }
                return 2;
            });

        childLinks.append("title").text(d => {
            if (d.weight !== null && !isNaN(d.weight)) {
                return `${d.name} → ${genealogy.focus.name}\nWeight: ${d.weight.toFixed(4)}`;
            }
            return `${d.name} → ${genealogy.focus.name}`;
        });
    }

    // Draw nodes
    drawNodes(parentPositions, sizeScale, "parent");
    drawNodes([focusPosition], sizeScale, "focus");
    drawNodes(childPositions, sizeScale, "child");
}

function drawNodes(nodes, sizeScale, nodeType) {
    const nodeGroups = container.selectAll(`.node-${nodeType}`)
        .data(nodes)
        .enter()
        .append("g")
        .attr("class", `node node-${nodeType}`)
        .attr("transform", d => `translate(${d.x}, ${d.y})`)
        .style("cursor", "pointer");

    // Draw circles
    nodeGroups.append("circle")
        .attr("r", d => sizeScale(d.size))
        .style("fill", d => {
            // Tree nodes get a distinct color (green-ish)
            if (d.isTreeNode) return "#28a745";
            return d.fill || "#69b3a2";
        })
        .style("stroke", d => {
            if (nodeType === "focus") return "#000";
            // Tree nodes get darker stroke
            if (d.isTreeNode) return "#1e7e34";
            return "#555";
        })
        .style("stroke-width", d => {
            if (nodeType === "focus") return 3;
            // Tree nodes get thicker stroke
            if (d.isTreeNode) return 2.5;
            return 1.5;
        })
        .on("click", function(event, d) {
            event.stopPropagation();
            changeFocus(d.name);
        })
        .on("mouseover", function(event, d) {
            const currentWidth = d3.select(this).style("stroke-width");
            d3.select(this).style("stroke-width", parseFloat(currentWidth) + 2);
        })
        .on("mouseout", function(event, d) {
            if (nodeType === "focus") {
                d3.select(this).style("stroke-width", 3);
            } else if (d.isTreeNode) {
                d3.select(this).style("stroke-width", 2.5);
            } else {
                d3.select(this).style("stroke-width", 1.5);
            }
        });

    // Draw labels
    nodeGroups.append("text")
        .attr("dy", d => {
            // For focus node, place label above the node
            if (nodeType === "focus") return -25;
            // For others, center vertically
            return 3;
        })
        .attr("x", d => {
            if (nodeType === "parent") return -25;
            if (nodeType === "child") return 25;
            return 0;
        })
        .style("text-anchor", d => {
            if (nodeType === "parent") return "end";
            if (nodeType === "child") return "start";
            return "middle";
        })
        .style("font-size", nodeType === "focus" ? "14pt" : "12pt")
        .style("font-weight", nodeType === "focus" ? "bold" : "normal")
        .style("fill", nodeType === "focus" ? "#000" : "#0066cc")
        .style("cursor", "pointer")
        .text(d => d.name)
        .on("click", function(event, d) {
            event.stopPropagation();
            changeFocus(d.name);
        })
        .on("mouseover", function(event, d) {
            d3.select(this).style("fill", nodeType === "focus" ? "#000" : "#0044aa");
        })
        .on("mouseout", function(event, d) {
            d3.select(this).style("fill", nodeType === "focus" ? "#000" : "#0066cc");
        });

    // Add tooltips
    nodeGroups.append("title").text(d => {
        const typeLabel = nodeType === "parent" ? "Parent genre" :
            nodeType === "child" ? "Subgenre" :
            "Focused genre";
        const treeLabel = d.isTreeNode ? " [TREE]" : "";
        return `${d.name}${treeLabel} (${typeLabel}) - Size: ${d.size.toFixed(2)}`;
    });
}

// Start the visualization
render();