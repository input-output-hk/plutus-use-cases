/* eslint-disable */
/* tslint:disable */
/**
 * This is an autogenerated file created by the Stencil compiler.
 * It contains typing information for all components that exist in this project.
 */
import { HTMLStencilElement, JSXBase } from "@stencil/core/internal";
export namespace Components {
    interface CSwap {
    }
}
declare global {
    interface HTMLCSwapElement extends Components.CSwap, HTMLStencilElement {
    }
    var HTMLCSwapElement: {
        prototype: HTMLCSwapElement;
        new (): HTMLCSwapElement;
    };
    interface HTMLElementTagNameMap {
        "c-swap": HTMLCSwapElement;
    }
}
declare namespace LocalJSX {
    interface CSwap {
    }
    interface IntrinsicElements {
        "c-swap": CSwap;
    }
}
export { LocalJSX as JSX };
declare module "@stencil/core" {
    export namespace JSX {
        interface IntrinsicElements {
            "c-swap": LocalJSX.CSwap & JSXBase.HTMLAttributes<HTMLCSwapElement>;
        }
    }
}
