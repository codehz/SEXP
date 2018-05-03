[@bs.module]
external jSReactClass : ReasonReact.reactClass = "./contenteditable.js";

let make =
    (
      ~html: string,
      ~autofocus: bool,
      ~className: string,
      ~onChange: ReactEventRe.Form.t => unit,
      ~onKeyUp: ReactEventRe.Keyboard.t => unit,
      ~onKeyDown: ReactEventRe.Keyboard.t => unit,
      ~onBlur: ReactEventRe.Focus.t => unit,
      ~onFocus: ReactEventRe.Focus.t => unit,
      children,
    ) =>
  ReasonReact.wrapJsForReason(
    ~reactClass=jSReactClass,
    ~props={
      "html": html,
      "autofocus": autofocus,
      "className": className,
      "onKeyUp": onKeyUp,
      "onKeyDown": onKeyDown,
      "onBlur": onBlur,
      "onChange": onChange,
      "onFocus": onFocus,
    },
    children,
  );