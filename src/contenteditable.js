const React = require("react");

let stripNbsp = str => str.replace(/&nbsp;|\u202F|\u00A0/g, ' ');

const test = document.createElement('span');

function xesc(unsafe) {
  test.innerText = unsafe;
  return test.innerHTML;
}

module.exports = class ContentEditable extends React.Component {
  constructor() {
    super();
    this.emitChange = this.emitChange.bind(this);
  }

  render() {
    var { tagName, html, init, autofocus, ...props } = this.props;

    return React.createElement(
      tagName || 'div',
      {
        ...props,
        ref: (e) => {
          this.htmlEl = e;
          if (e && autofocus) {
            e.focus();
          }
        },
        onInput: this.emitChange,
        onBlur: this.props.onBlur || this.emitChange,
        contentEditable: true,
        dangerouslySetInnerHTML: {__html: xesc(html)}
      },
      this.props.children);
  }

  shouldComponentUpdate(nextProps) {
    let { props, htmlEl } = this;

    // We need not rerender if the change of props simply reflects the user's edits.
    // Rerendering in this case would make the cursor/caret jump

    // Rerender if there is no element yet... (somehow?)
    if (!htmlEl) {
      return true;
    }

    // ...or if html really changed... (programmatically, not by user edit)
    if (
      stripNbsp(xesc(nextProps.html)) !== stripNbsp(htmlEl.innerHTML) &&
      xesc(nextProps.html) !== xesc(props.html)
    ) {
      return true;
    }

    let optional = ['style', 'className', 'disabled', 'tagName', 'autofocus'];

    // Handle additional properties
    return optional.some(name => props[name] !== nextProps[name]);
  }

  componentDidUpdate() {
    if ( this.htmlEl && this.props.html !== this.htmlEl.innerHTML ) {
      // Perhaps React (whose VDOM gets outdated because we often prevent
      // rerendering) did not update the DOM. So we update it manually now.
      this.htmlEl.innerHTML = xesc(this.props.html);
    }
  }

  emitChange(evt) {
    if (!this.htmlEl) return;
    var text = this.htmlEl.innerText;
    if (this.props.onChange && text !== this.lastText) {
      // Clone event with Object.assign to avoid 
      // "Cannot assign to read only property 'target' of object"
      var evt = Object.assign({}, evt, { 
        target: { 
          value: text
        } 
      });
      this.props.onChange(evt);
    }
    this.lastText = text;
  }
}
