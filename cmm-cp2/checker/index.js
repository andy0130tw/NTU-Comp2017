/*
 * a wrapper for vis.js used on http://webgraphviz.org/
 */

const fs = require('fs');
const viz = require('./viz');

const input = fs.readFileSync('/dev/stdin', 'utf-8');

// cannot surpress the output!?
viz.Viz(input, 'svg');
