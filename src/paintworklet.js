registerPaint('bracket', class {
  static get inputProperties() {
    return [
      '--bracket-color',
      '--bracket-background-color',
      '--connection-color',
      '--bracket-radius'
    ]
  }
  paint(ctx, size, style) {
    const bracketColor = style.get('--bracket-color').toString() || 'black';
    const backgroundColor = style.get('--bracket-background-color').toString() || 'white';
    const connectionColor = style.get('--connection-color').toString() || 'rgba(0, 0, 0, 0.2)';
    const radius = parseInt(style.get('--bracket-radius').toString()) || 9;
    ctx.strokeStyle = bracketColor;
    ctx.fillStyle = backgroundColor;
    ctx.lineWidth = 2;
    ctx.beginPath();
    ctx.arc(size.width - radius - 1, radius + 1, radius, -0.5 * Math.PI, 0 * Math.PI);
    ctx.arc(size.width - radius - 1, size.height - radius - 1, radius, 0, 0.5 * Math.PI);
    ctx.fill();
    ctx.stroke();
    ctx.beginPath();
    ctx.arc(radius + 1, size.height - radius - 1, radius, 0.5 * Math.PI, 1 * Math.PI);
    ctx.arc(radius + 1, radius + 1, radius, 3 * Math.PI, 1.5 * Math.PI);
    ctx.fill();
    ctx.stroke();
    ctx.strokeStyle = connectionColor;
    ctx.fillRect(radius + 1, 1, size.width - radius * 2 - 2, size.height - 2)
    ctx.beginPath()
    ctx.moveTo(radius + 1, 1)
    ctx.lineTo(size.width - radius - 1, 1)
    ctx.moveTo(radius + 1, size.height - 1)
    ctx.lineTo(size.width - radius - 1, size.height - 1)
    ctx.stroke();
  }
});

registerPaint('quote', class {
  static get inputProperties() {
    return [
      '--quote-color',
      '--quote-background-color',
      '--quote-radius',
    ]
  }
  paint(ctx, size, style) {
    const quoteColor = style.get('--quote-color').toString() || 'black';
    const backgroundColor = style.get('--quote-background-color').toString() || 'white';
    const radius = parseInt(style.get('--quote-radius').toString()) || 9;
    ctx.strokeStyle = quoteColor;
    ctx.fillStyle = backgroundColor;
    ctx.lineWidth = 2;
    ctx.beginPath();
    // ctx.moveTo(1, 1);
    ctx.arc(size.width - radius - 1, radius + 1, radius, -0.5 * Math.PI, 0 * Math.PI);
    ctx.arc(size.width - radius - 1, size.height - radius - 1, radius, 0, 0.5 * Math.PI);
    ctx.arc(radius + 1, size.height - radius - 1, radius, 0.5 * Math.PI, 1 * Math.PI);
    ctx.arc(radius + 1, radius + 1, radius, 3 * Math.PI, 1.5 * Math.PI);
    // ctx.lineTo(1, size.height - 1);
    ctx.closePath();
    ctx.fill();
    ctx.stroke();
  }
});

registerPaint('string-quote', class {
  static get inputProperties() {
    return [
      '--string-quote-color',
      '--string-quote-offset',
    ]
  }
  paint(ctx, size, style) {
    const quoteColor = style.get('--string-quote-color').toString() || 'black';
    const quoteOffset = parseInt(style.get('--string-quote-offset').toString()) || 0;
    ctx.strokeStyle = quoteColor;
    ctx.lineWidth = 2;
    ctx.beginPath();
    ctx.moveTo(1, 1);
    ctx.lineTo(1 + quoteOffset, 5);
    ctx.moveTo(4, 1);
    ctx.lineTo(4 + quoteOffset, 5);
    ctx.moveTo(size.width - 1, 1);
    ctx.lineTo(size.width - 1 - quoteOffset, 5);
    ctx.moveTo(size.width - 4, 1);
    ctx.lineTo(size.width - 4 - quoteOffset, 5);
    ctx.stroke();
  }
});