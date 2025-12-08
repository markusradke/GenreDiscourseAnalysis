// Static tree plot for publication
// Supports horizontal and vertical layouts with automatic label rotation

const config = {
    width: data.width || 800,
    height: data.height || 600,
    layout: data.layout || "horizontal",
    fontSize: data.font_size || 10,
    marginLeft: data.margin_left || 100,
    marginRight: data.margin_right || 100,
    marginTop: data.margin_top || 50,
    marginBottom: data.margin_bottom || 50,
    linkColor: "#cccccc",
    labelColor: "#000000",
    minLabelSpacing: 2,
    horizontalLabelLevels: data.horizontal_label_levels,
    spacingFactor: data.spacing_factor == null ? 0 : Number(data.spacing_factor)
};

const treeData = data.tree;
const weights = buildWeightsLookup(data.weights);
let horizontalLabelDepths = {};

function render() {
    const root = d3.hierarchy(treeData);
    sortBySize(root);

    const charWidth = config.fontSize * 0.6;
    const charHeight = config.fontSize * 1.2;

    const minDimensions = calculateMinDimensions(root, charWidth, charHeight);
    checkDimensionsAndWarn(minDimensions);

    const innerWidth = config.width - config.marginLeft - config.marginRight;
    const innerHeight = config.height - config.marginTop - config.marginBottom;

    // Reserve space for labels at root and leaf ends
    const maxLabelLength = d3.max(root.descendants(), d => d.data.name.length);
    const labelReserve = maxLabelLength * charWidth + 20;

    const layoutWidth = Math.max(10, innerWidth - labelReserve);
    const layoutHeight = Math.max(10, innerHeight - labelReserve);

    const treeLayout = d3.tree()
        .size(config.layout === "horizontal" ? [innerHeight, layoutWidth] : [layoutWidth, innerHeight])
        .separation((a, b) => a.parent === b.parent ? 1 : 1.2);

    treeLayout(root);

    root.each(node => {
        node.originalX = node.x;
        node.originalY = node.y;
    });

    // Determine which levels can have horizontal labels (vertical layout only)
    if (config.layout === "vertical") {
        horizontalLabelDepths = detectHorizontalLabelLevels(root, charWidth);
    }

    setupSvg();
    const {
        container,
        group: g
    } = createMainGroup();

    const sizeExtent = d3.extent(root.descendants(), d => d.data.size || 1);
    const sizeScale = d3.scaleLinear()
        .domain(sizeExtent)
        .range([3, 12]);

    const weightExtent = getWeightExtent(root);
    const strokeScale = d3.scaleLinear()
        .domain(weightExtent)
        .range([0.5, 4]);

    adjustDepthSpacing(root, sizeScale, charWidth, charHeight);
    applySpacingFactor(root);

    renderLinks(g, root, strokeScale);
    renderNodes(g, root, sizeScale);
    renderLabels(g, root, sizeScale, charWidth, charHeight);

    fitTreeWithinCanvas(container, g);
}

function buildWeightsLookup(weightsData) {
    const lookup = {};
    if (weightsData && weightsData.key) {
        weightsData.key.forEach((k, i) => {
            lookup[k] = weightsData.weight[i];
        });
    }
    return lookup;
}

function sortBySize(node) {
    if (node.children) {
        node.children.sort((a, b) => totalSize(b) - totalSize(a));
        node.children.forEach(sortBySize);
    }
}

function totalSize(node) {
    let sum = node.data.size || 0;
    if (node.children) {
        sum += node.children.reduce((acc, c) => acc + totalSize(c), 0);
    }
    return sum;
}

function calculateMinDimensions(root, charWidth, charHeight) {
    const nodesByDepth = {};
    root.descendants().forEach(d => {
        if (!nodesByDepth[d.depth]) nodesByDepth[d.depth] = [];
        nodesByDepth[d.depth].push(d);
    });

    let maxNodesAtDepth = 0;
    let maxLabelLength = 0;
    let totalDepths = Object.keys(nodesByDepth).length;

    Object.values(nodesByDepth).forEach(nodes => {
        maxNodesAtDepth = Math.max(maxNodesAtDepth, nodes.length);
        nodes.forEach(n => {
            maxLabelLength = Math.max(maxLabelLength, n.data.name.length);
        });
    });

    const labelPixelLength = maxLabelLength * charWidth;
    const spacing = config.minLabelSpacing;

    if (config.layout === "horizontal") {
        const minHeight = maxNodesAtDepth * (charHeight + spacing) +
            config.marginTop + config.marginBottom;
        const minWidth = totalDepths * (labelPixelLength + 40) +
            config.marginLeft + config.marginRight;
        return {
            minWidth,
            minHeight,
            maxNodesAtDepth,
            maxLabelLength,
            totalDepths
        };
    } else {
        const minWidth = maxNodesAtDepth * (charHeight + spacing) +
            config.marginLeft + config.marginRight;
        const minHeight = totalDepths * (labelPixelLength + 40) +
            config.marginTop + config.marginBottom;
        return {
            minWidth,
            minHeight,
            maxNodesAtDepth,
            maxLabelLength,
            totalDepths
        };
    }
}

function checkDimensionsAndWarn(minDimensions) {
    const warnings = [];

    if (config.width < minDimensions.minWidth) {
        warnings.push(
            `Width ${config.width}px may be too small. ` +
            `Recommended minimum: ${Math.ceil(minDimensions.minWidth)}px ` +
            `(${minDimensions.maxNodesAtDepth} nodes at widest level)`
        );
    }

    if (config.height < minDimensions.minHeight) {
        warnings.push(
            `Height ${config.height}px may be too small. ` +
            `Recommended minimum: ${Math.ceil(minDimensions.minHeight)}px ` +
            `(${minDimensions.totalDepths} hierarchy levels)`
        );
    }

    if (warnings.length > 0) {
        console.warn("Tree plot dimension warnings:\n" + warnings.join("\n"));
    }
}

function setupSvg() {
    svg.selectAll("*").remove();
    svg.attr("width", config.width)
        .attr("height", config.height)
        .style("background-color", "white")
        .style("font-family", "Arial, sans-serif");
}

function createMainGroup() {
    const container = svg.append("g");
    const group = container.append("g");
    return {
        container,
        group
    };
}

function getWeightExtent(root) {
    const linkWeights = root.links().map(link => {
        const key = `${link.target.data.name}->${link.source.data.name}`;
        return weights[key];
    }).filter(w => w != null);

    return linkWeights.length > 0 ? d3.extent(linkWeights) : [1, 1];
}

function detectHorizontalLabelLevels(root, charWidth) {
    const result = {};

    // If user specified number of levels, use that
    if (config.horizontalLabelLevels != null && config.horizontalLabelLevels >= 0) {
        root.descendants().forEach(d => {
            result[d.depth] = d.depth < config.horizontalLabelLevels;
        });
        return result;
    }

    // Otherwise, auto-detect based on overlap
    const nodesByDepth = {};
    root.descendants().forEach(d => {
        if (!nodesByDepth[d.depth]) nodesByDepth[d.depth] = [];
        nodesByDepth[d.depth].push(d);
    });

    Object.keys(nodesByDepth).forEach(depth => {
        const nodes = nodesByDepth[depth].sort((a, b) => a.x - b.x);

        if (nodes.length === 1) {
            result[depth] = true;
            return;
        }

        let hasOverlap = false;
        for (let i = 0; i < nodes.length - 1; i++) {
            const current = nodes[i];
            const next = nodes[i + 1];

            const currentLabelWidth = current.data.name.length * charWidth;
            const nextLabelWidth = next.data.name.length * charWidth;

            // Check if labels would overlap (centered on node)
            const currentRight = current.x + currentLabelWidth / 2;
            const nextLeft = next.x - nextLabelWidth / 2;
            const minGap = 8;

            if (currentRight + minGap > nextLeft) {
                hasOverlap = true;
                break;
            }
        }

        result[depth] = !hasOverlap;
    });

    return result;
}

function getNodeCoords(d) {
    return config.layout === "horizontal" ? {
        x: d.y,
        y: d.x
    } : {
        x: d.x,
        y: d.y
    };
}

function renderLinks(g, root, strokeScale) {
    const linkGenerator = config.layout === "horizontal" ?
        d3.linkHorizontal().x(d => d.y).y(d => d.x) :
        d3.linkVertical().x(d => d.x).y(d => d.y);

    g.selectAll(".link")
        .data(root.links())
        .enter()
        .append("path")
        .attr("class", "link")
        .attr("d", linkGenerator)
        .style("fill", "none")
        .style("stroke", config.linkColor)
        .style("stroke-width", d => {
            const key = `${d.target.data.name}->${d.source.data.name}`;
            const w = weights[key];
            return w != null ? strokeScale(w) : 1.5;
        });
}

function renderNodes(g, root, sizeScale) {
    g.selectAll(".node")
        .data(root.descendants())
        .enter()
        .append("g")
        .attr("class", "node")
        .attr("transform", d => {
            const coords = getNodeCoords(d);
            return `translate(${coords.x}, ${coords.y})`;
        })
        .append("circle")
        .attr("r", d => sizeScale(d.data.size || 1))
        .style("fill", d => d.data.fill || "#808080")
        .style("stroke", "none");
}

function renderLabels(g, root, sizeScale, charWidth, charHeight) {
    g.selectAll(".label")
        .data(root.descendants())
        .enter()
        .append("text")
        .attr("class", "label")
        .attr("transform", d => getLabelTransform(d, sizeScale))
        .attr("text-anchor", d => getLabelAnchor(d))
        .attr("dominant-baseline", d => getLabelBaseline(d))
        .style("font-size", `${config.fontSize}pt`)
        .style("font-weight", d => d.children && d.children.length > 0 ? "bold" : "normal")
        .style("fill", config.labelColor)
        .text(d => d.data.name);
}

function fitTreeWithinCanvas(container, group) {
    const node = group.node();
    if (!node) {
        return;
    }

    const bbox = node.getBBox();
    const innerWidth = config.width - config.marginLeft - config.marginRight;
    const innerHeight = config.height - config.marginTop - config.marginBottom;

    // Calculate scale to fit content within available space
    const scaleX = bbox.width > 0 ? innerWidth / bbox.width : 1;
    const scaleY = bbox.height > 0 ? innerHeight / bbox.height : 1;
    const scale = Math.min(scaleX, scaleY, 1);

    // Calculate translation to center the (possibly scaled) content
    const scaledWidth = bbox.width * scale;
    const scaledHeight = bbox.height * scale;

    const tx = config.marginLeft + (innerWidth - scaledWidth) / 2 - bbox.x * scale;
    const ty = config.marginTop + (innerHeight - scaledHeight) / 2 - bbox.y * scale;

    if (scale < 1 && console && console.warn) {
        console.warn(
            `Tree content scaled to ${Math.round(scale * 100)}% to fit. ` +
            `Content: ${Math.round(bbox.width)}x${Math.round(bbox.height)}px, ` +
            `Available: ${Math.round(innerWidth)}x${Math.round(innerHeight)}px.`
        );
    }

    container.attr("transform", `translate(${tx}, ${ty}) scale(${scale})`);
}

function usesHorizontalLabels(d) {
    if (config.layout === "horizontal") {
        return true;
    }
    if (config.layout === "vertical" && horizontalLabelDepths[d.depth]) {
        return true;
    }
    return false;
}

function getLabelTransform(d, sizeScale) {
    const coords = getNodeCoords(d);
    const nodeRadius = sizeScale(d.data.size || 1);
    const labelOffset = nodeRadius + 4;
    const isLeaf = !d.children || d.children.length === 0;
    const useHorizontal = usesHorizontalLabels(d);

    if (config.layout === "horizontal") {
        const x = isLeaf ? coords.x + labelOffset : coords.x - labelOffset;
        return `translate(${x}, ${coords.y})`;
    } else if (useHorizontal) {
        // Vertical layout with horizontal labels
        const y = isLeaf ? coords.y + labelOffset : coords.y - labelOffset;
        return `translate(${coords.x}, ${y})`;
    } else {
        // Vertical layout with rotated labels
        const y = isLeaf ? coords.y + labelOffset : coords.y - labelOffset;
        return `translate(${coords.x}, ${y}) rotate(-90)`;
    }
}

function getLabelAnchor(d) {
    const isLeaf = !d.children || d.children.length === 0;
    const useHorizontal = usesHorizontalLabels(d);

    if (config.layout === "horizontal") {
        return isLeaf ? "start" : "end";
    } else if (useHorizontal) {
        // Vertical with horizontal labels: center them
        return "middle";
    } else {
        return isLeaf ? "end" : "start";
    }
}

function getLabelBaseline(d) {
    const isLeaf = !d.children || d.children.length === 0;
    const useHorizontal = usesHorizontalLabels(d);

    if (config.layout === "horizontal") {
        return "middle";
    } else if (useHorizontal) {
        // Vertical with horizontal labels
        return isLeaf ? "hanging" : "auto";
    } else {
        return "middle";
    }
}

function adjustDepthSpacing(root, sizeScale, charWidth, charHeight) {
    if (Math.abs(Number(config.spacingFactor) - 1) < 1e-9) {
        return;
    }

    const axis = config.layout === "vertical" ? "y" : "x";
    const available = config.layout === "vertical" ?
        config.height - config.marginTop - config.marginBottom :
        config.width - config.marginLeft - config.marginRight;

    const maxDepth = d3.max(root.descendants(), d => d.depth);
    const depthSpacing = new Array(maxDepth).fill(0);
    const gapPadding = 4;

    root.descendants().forEach(node => {
        if (!node.children) {
            return;
        }
        const parentExtents = getDepthExtents(node, sizeScale, charWidth, charHeight);
        node.children.forEach(child => {
            const childExtents = getDepthExtents(child, sizeScale, charWidth, charHeight);
            const required = parentExtents.forward + childExtents.backward + gapPadding;
            depthSpacing[node.depth] = Math.max(depthSpacing[node.depth], required);
        });
    });

    const positions = [0];
    for (let depth = 0; depth < depthSpacing.length; depth += 1) {
        const gap = depthSpacing[depth] || (charHeight + gapPadding);
        positions[depth + 1] = positions[depth] + gap;
    }

    const totalRequired = positions[positions.length - 1] || 0;
    let scale = 1;
    if (totalRequired > 0 && available < totalRequired) {
        scale = available / totalRequired;
    }

    if (totalRequired > available && console && console.warn) {
        console.warn(
            `Available ${axis}-space ${available}px is smaller than required ${totalRequired}px. ` +
            "Labels may overlap. Consider increasing plot size."
        );
    }

    const effectiveSpan = totalRequired * scale;
    const offset = (available - effectiveSpan) / 2;

    root.descendants().forEach(node => {
        const basePosition = positions[node.depth] || 0;
        node[axis] = offset + basePosition * scale;
    });
}

function getDepthExtents(node, sizeScale, charWidth, charHeight) {
    const radius = sizeScale(node.data.size || 1);
    const hasChildren = node.children && node.children.length > 0;
    const labelSpan = getLabelSpanAlongDepth(node, charWidth, charHeight);

    return {
        backward: radius + (hasChildren ? labelSpan : 0),
        forward: radius + (hasChildren ? 0 : labelSpan)
    };
}

function getLabelSpanAlongDepth(node, charWidth, charHeight) {
    const nameLength = node.data.name.length;
    const useHorizontal = usesHorizontalLabels(node);

    if (config.layout === "vertical") {
        return useHorizontal ? charHeight : nameLength * charWidth;
    }

    // Horizontal layout
    return nameLength * charWidth;
}

function applySpacingFactor(root) {
    const factor = Number(config.spacingFactor);
    if (!Number.isFinite(factor) || factor === 0) {
        return;
    }

    const axis = config.layout === "vertical" ? "y" : "x";

    root.descendants().forEach(node => {
        const original = config.layout === "vertical" ? node.originalY : node.originalX;
        const tight = node[axis];
        if (original == null || tight == null) {
            return;
        }
        node[axis] = tight + factor * (original - tight);
    });
}

render();