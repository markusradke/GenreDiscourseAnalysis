const margin = {
    left: data.margin_left,
    top: data.margin_top,
    bottom: data.margin_bottom,
    right: data.margin_right
};

const root = d3.hierarchy(data.tree);
const treeLayout = d3.tree().size([height - margin.bottom, width - margin.right]);
treeLayout(root);

const minFontSize = data.minFontSize;
const maxFontSize = data.maxFontSize;
const fontSizeScale = d3.scaleLinear()
    .domain([0, d3.max(root.descendants(), d => d.data.size)])
    .range([minFontSize, maxFontSize]) // 

svg.selectAll('.link')
    .data(root.links())
    .enter()
    .append('path')
    .attr('class', 'link')
    .attr('d', d3.linkHorizontal()
        .x(d => d.y + margin.left)
        .y(d => d.x + margin.top))
    .style('fill', 'none')
    .style('stroke', '#727272ff')
    .style('stroke-width', 1.5)
    .style('stroke-linecap', 'round');

const node = svg.selectAll('.node')
    .data(root.descendants())
    .enter()
    .append('g')
    .attr('class', 'node')
    .attr('transform', d => `translate(${d.y + margin.left}, ${d.x + margin.top})`); // Corrected template literal

node.append('circle')
    .attr('r', d => d.data.size)
    .style('fill', d => d.data.fill);

node.append('text')
    .attr('dy', 3)
    .attr('x', d => d.children ? -d.data.size - 5 : d.data.size + 5)
    .style('text-anchor', d => d.children ? 'end' : 'start')
    .style('font-weight', d => d.children ? 'bold' : 'normal')
    .text(d => d.data.name)
    .style('font-size', d => fontSizeScale(d.data.size));