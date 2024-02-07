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
    arrow_func_start:{ // place the arrow above the letter
        symbol: '→',
        options: {
            color: getColors("number"),
            textDecoration: `none;
                font-family: "NewComputerModernMath";
                transform: translate(0.84em, -0.9em);
                font-size: 0.8em;
                display: inline-block; `
        }
    },
    arrow_func_end: empty,
    abs_func: { // display the bar
        symbol: '|',
        options: {
            color: getColors("operator"),
            textDecoration: `none;`
        }
    },
    overline_func_start: { // place the overline above the letter
        symbol: '\u0305',
        options: {
            color: getColors("number"),
            textDecoration: `none;
            transform: translate(0em, -0.2em);
            display: inline-block;`
        }
    },
    overline_func_end: empty,
    dot_func_start: { // place the dot above the letter
        symbol: '⋅',
        options: {
            color: getColors("number"),
            textDecoration: `none;
            transform: translate(0.43em, -0.52em);
            display: inline-block;`
        }
    },
    dot_func_end: empty,
    double_dot_func_start: { // place the double dot above the letter
        symbol: '¨',
        options: {
            color: getColors("number"),
            textDecoration: `none;
            transform: translate(0.55em, -0.25em);
            display: inline-block;`
        }
    },
    double_dot_func_end: empty,
    triple_dot_func_start: { // place the triple dot above the letter
        symbol: '\u20DB',
        options: {
            color: getColors("number"),
            textDecoration: `none;
            font-family: JuliaMono;
            font-size: 1.4em;
            transform: translate(-0.1em);
            display: inline-block;`
        }
    },
    triple_dot_func_end: empty,
    quad_dot_func_start: { // place the quad dot above the letter
        symbol: '\u20DC',
        options: {
            color: getColors("number"),
            textDecoration: `none;
            font-family: JuliaMono;
            font-size: 1.4em;
            transform: translate(-0.1em);
            display: inline-block;`
        }
    },
    quad_dot_func_end: empty,
    hat_func_start: { // place the hat above the letter
        symbol: '^',
        options: {
            color: getColors("number"),
            textDecoration: `none;
            transform: translate(0.6em, -0.5em);
            font-size: 0.9em;
            display: inline-block;`
        }
    },
    hat_func_end: empty,
    tilde_func_start: { // place the tilde above the letter
        symbol: '~',
        options: {
            color: getColors("number"),
            textDecoration: `none;
            transform: translate(0.59em, -0.7em);
            font-size: 0.9em;
            display: inline-block;`
        }
    },
    tilde_func_end: empty,
    norm_func: { // display the double bar
        symbol: '‖',
        options: {
            color: getColors("operator"),
            textDecoration: `none;`
        }
    },
};