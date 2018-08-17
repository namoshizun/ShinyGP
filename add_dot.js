function(el, x) {
    Number.prototype.between = function(min, max) {
        return this >= min && this <= max;
    };

    Plotly.d3.select('.plotly').on('click', function(d, i) {
        const e = Plotly.d3.event;
        const bg = document.getElementsByClassName('bg')[0];
        const [minX, maxX] = [...el.layout.xaxis.range];
        const [minY, maxY] = [...el.layout.yaxis.range];

        // Convert pixel location to the coordinate
        const x = ((e.layerX - bg.attributes['x'].value + 4) / (bg.attributes['width'].value)) * (maxX - minX) + minX;
        const y = ((e.layerY - bg.attributes['y'].value + 4) / (bg.attributes['height'].value)) * (minY - maxY) + maxY;
        if (x.between(minX, maxX) && y.between(minY, maxY)) {
            // Notify Shiny backend of the clicked coordinate
            // Sending {x, y} message to the handler of event 'addObs'
            Shiny.onInputChange('addObs', { x, y })
        }
    });
}