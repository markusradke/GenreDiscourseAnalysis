// Interactive Network Plot
const fixedHeight = data.height;
const fixedWidth = 1600;
const margin = 100;

// Initialize SVG
svg.attr("width", fixedWidth)
    .attr("height", fixedHeight)
    .style("overflow", "auto");

// Create container for tree
const container = svg.append("g")
    .attr("class", "tree-container")
    .attr("transform", `translate(${margin}, ${margin})`);

// Copy data and create persistent hierarchy
let currentData = JSON.parse(JSON.stringify(data.tree));
let hierarchyRoot = null; // Keep the hierarchy root persistent
let isFirstRender = true; // Track if this is the first render

// Collapse function for raw data (not used anymore but keeping for reference)
function collapseToFirstLevel(node, depth = 0) {
    if (depth >= 1 && node.children) {
        node._children = node.children;
        node.children = null;
    }
    if (node.children) {
        node.children.forEach(child => collapseToFirstLevel(child, depth + 1));
    }
}

// Collapse function for D3 hierarchy nodes (the one we actually use)
function collapseHierarchyToFirstLevel(node, depth = 0) {

    if (depth >= 1 && node.children) {
        node._children = node.children;
        node.children = null;
    }

    // Continue recursively for visible children
    if (node.children) {
        node.children.forEach(child => collapseHierarchyToFirstLevel(child, depth + 1));
    }

    // Also process hidden children to ensure deep collapse
    if (node._children) {
        node._children.forEach(child => collapseHierarchyToFirstLevel(child, depth + 1));
    }
}

// Find a node in the hierarchy by name
function findNodeByName(root, name) {
    if (root.data.name === name) {
        return root;
    }
    if (root.children) {
        for (let child of root.children) {
            const found = findNodeByName(child, name);
            if (found) return found;
        }
    }
    if (root._children) {
        for (let child of root._children) {
            const found = findNodeByName(child, name);
            if (found) return found;
        }
    }
    return null;
}

// Check if a node has expandable children in persistent hierarchy
function hasExpandableChildren(nodeName) {
    if (!nodeName || !hierarchyRoot) {
        console.warn("hasExpandableChildren: Invalid parameters", {
            nodeName,
            hierarchyRoot: !!hierarchyRoot
        });
        return false;
    }
    const persistentNode = findNodeByName(hierarchyRoot, nodeName);
    return persistentNode && (persistentNode.children || persistentNode._children);
}

// Check if a node is collapsed in persistent hierarchy
function isCollapsed(nodeName) {
    if (!nodeName || !hierarchyRoot) {
        console.warn("isCollapsed: Invalid parameters", {
            nodeName,
            hierarchyRoot: !!hierarchyRoot
        });
        return false;
    }
    const persistentNode = findNodeByName(hierarchyRoot, nodeName);
    return persistentNode && persistentNode._children && !persistentNode.children;
}

// Toggle function
function toggle(d) {
    // Safety check for data structure
    if (!d.data) {
        console.error('Node data is undefined, skipping toggle');
        return;
    }
    // Find the corresponding node in our persistent hierarchy
    const persistentNode = findNodeByName(hierarchyRoot, d.data.name);
    if (!persistentNode) {
        console.error('Could not find node in persistent hierarchy:', d.data.name);
        return;
    }

    if (persistentNode.children) {
        persistentNode._children = persistentNode.children;
        persistentNode.children = null;
    } else if (persistentNode._children) {
        persistentNode.children = persistentNode._children;
        persistentNode._children = null;
    }

    update();
}

// Update function
function update() {
    // Clear previous content
    container.selectAll("*").remove();

    // Create or use existing hierarchy
    if (isFirstRender) {
        hierarchyRoot = d3.hierarchy(currentData);
        collapseHierarchyToFirstLevel(hierarchyRoot, 0);
        isFirstRender = false;
    }

    const root = hierarchyRoot;
    const treeLayout = d3.tree().size([fixedHeight - 2 * margin, fixedWidth - 2 * margin]);
    treeLayout(root);

    // Scale for node sizes
    const sizeScale = d3.scaleLinear()
        .domain([0, d3.max(root.descendants(), d => d.data.size) || 1])
        .range([4, 15]);

    // Create links
    const links = container.selectAll('.link')
        .data(root.links())
        .enter()
        .append('path')
        .attr('class', 'link')
        .attr('d', d3.linkHorizontal()
            .x(d => d.y)
            .y(d => d.x))
        .style('fill', 'none')
        .style('stroke', '#ccc')
        .style('stroke-width', 2);

    // Create node groups
    const nodes = container.selectAll('.node')
        .data(root.descendants())
        .enter()
        .append('g')
        .attr('class', 'node')
        .attr('transform', d => `translate(${d.y}, ${d.x})`)
        .style('cursor', d => (d.data && hasExpandableChildren(d.data.name)) ? 'pointer' : 'default');

    // Add circles
    nodes.append('circle')
        .attr('r', d => sizeScale(d.data.size))
        .style('fill', d => d.data.fill || '#69b3a2')
        .style('stroke', d => (d.data && hasExpandableChildren(d.data.name)) ? '#000' : 'none')
        .style('stroke-width', d => (d.data && hasExpandableChildren(d.data.name)) ? 2 : 1)
        .on('click', function(event, d) {
            // Check persistent hierarchy instead of display node
            const persistentNode = findNodeByName(hierarchyRoot, d.data.name);
            if (persistentNode) {
                if (persistentNode._children || persistentNode.children) {
                    toggle(d);
                }
            }
        })
        .on('mouseover', function(event, d) {
            if (d.data && hasExpandableChildren(d.data.name)) {
                d3.select(this).style('stroke-width', 4);
            }
        })
        .on('mouseout', function(event, d) {
            d3.select(this).style('stroke-width', (d.data && hasExpandableChildren(d.data.name)) ? 2 : 1);
        });

    // Add +/- indicators
    nodes.filter(d => d.data && hasExpandableChildren(d.data.name))
        .append('text')
        .attr('text-anchor', 'middle')
        .attr('dy', 4)
        .style('font-size', '10px')
        .style('font-weight', 'bold')
        .style('fill', 'white')
        .style('pointer-events', 'none')
        .text(d => isCollapsed(d.data.name) ? '+' : '−');

    // Add labels
    nodes.append('text')
        .attr('dy', 3)
        .attr('x', d => hasExpandableChildren(d.data.name) ? -18 : 18)
        .style('text-anchor', d => hasExpandableChildren(d.data.name) ? 'end' : 'start')
        .style('font-size', '12pt')
        .style('font-weight', d => hasExpandableChildren(d.data.name) ? 'bold' : 'normal')
        .style('text-decoration', d => hasExpandableChildren(d.data.name) ? 'underline' : 'none')
        .style('fill', d => {
            if (hasExpandableChildren(d.data.name)) return '#0066cc'; // Expandable nodes = blue
            return '#333'; // Leaf nodes = dark gray
        })
        .style('cursor', d => hasExpandableChildren(d.data.name) ? 'pointer' : 'default')
        .text(d => d.data.name)
        .on('click', function(event, d) {
            // Check persistent hierarchy instead of display node
            const persistentNode = findNodeByName(hierarchyRoot, d.data.name);
            if (persistentNode) {
                if (persistentNode._children || persistentNode.children) {
                    toggle(d);
                }
            }
        })
        .on('mouseover', function(event, d) {
            if (hasExpandableChildren(d.data.name)) {
                d3.select(this).style('fill', '#0044aa');
            }
        })
        .on('mouseout', function(event, d) {
            d3.select(this).style('fill', hasExpandableChildren(d.data.name) ? '#0066cc' : '#333');
        });

    // Add tooltips
    nodes.append('title')
        .text(d => {
            if (hasExpandableChildren(d.data.name)) {
                return `${d.data.name} (Click to ${isCollapsed(d.data.name) ? 'expand' : 'collapse'})`;
            }
            return d.data.name;
        });
}

// Initial render
update();