import { getColors } from "./utils";

// helper variable to hide a text
const empty = {
    symbol: '',
    options: {
        color: "",
        textDecoration: `none;`
    }
};

export const STYLES = {
    arrow_func_start: empty,
    arrow_func_end: { // place the arrow above the letter
        symbol: '→',
        options: {
            color: getColors("number"),
            textDecoration: `none;
                font-family: "NewComputerModernMath";
                transform: translate(-0.84em, -0.9em);
                font-size: 0.8em;
                display: inline-block; `
        }
    },
    abs_func: { // display the bar
        symbol: '|',
        options: {
            color: getColors("operator"),
            textDecoration: `none;`
        }
    },
    overline_func_start: empty,
    overline_func_end: { // place the overline above the letter
        symbol: '\u0305',
        options: {
            color: getColors("number"),
            textDecoration: `none;
            transform: translate(-0.57em, -0.2em);
            display: inline-block;`
        }
    },
    dot_func_start: empty,
    dot_func_end: { // place the dot above the letter
        symbol: '⋅',
        options: {
            color: getColors("number"),
            textDecoration: `none;
            transform: translate(-0.43em, -0.52em);
            display: inline-block;`
        }
    },
    double_dot_func_start: empty,
    double_dot_func_end: { // place the double dot above the letter
        symbol: '¨',
        options: {
            color: getColors("number"),
            textDecoration: `none;
            transform: translate(-0.55em, -0.25em);
            display: inline-block;`
        }
    },
    triple_dot_func_start: empty,
    triple_dot_func_end: { // place the triple dot above the letter
        symbol: '\u20DB',
        options: {
            color: getColors("number"),
            textDecoration: `none;
            font-family: JuliaMono;
            font-size: 1.4em;
            transform: translate(-0.5em);
            display: inline-block;`
        }
    },
    quad_dot_func_start: empty,
    quad_dot_func_end: { // place the quad dot above the letter
        symbol: '\u20DC',
        options: {
            color: getColors("number"),
            textDecoration: `none;
            font-family: JuliaMono;
            font-size: 1.4em;
            transform: translate(-0.52em);
            display: inline-block;`
        }
    },
    hat_func_start: empty,
    hat_func_end: { // place the hat above the letter
        symbol: '^',
        options: {
            color: getColors("number"),
            textDecoration: `none;
            transform: translate(-0.6em, -0.5em);
            font-size: 0.9em;
            display: inline-block;`
        }
    },
    tilde_func_start: empty,
    tilde_func_end: { // place the tilde above the letter
        symbol: '~',
        options: {
            color: getColors("number"),
            textDecoration: `none;
            transform: translate(-0.59em, -0.7em);
            font-size: 0.9em;
            display: inline-block;`
        }
    },
    norm_func: { // display the double bar
        symbol: '‖',
        options: {
            color: getColors("operator"),
            textDecoration: `none;`
        }
    },
};