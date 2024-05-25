#[cfg(test)]
mod tests {
    use typst_math_rust::parse_document;

    #[test]
    fn basic_symbol() {
        let parsed = parse_document("$alpha$", -1, -1, 3, true, true, vec![], vec![]);
        assert_eq!(parsed.decorations.len(), 1);
        assert_eq!(parsed.decorations[0].symbol, "α");
        assert_eq!(parsed.decorations[0].uuid, "alpha");
    }

    #[test]
    fn symbol_repetition() {
        let parsed = parse_document("$alpha alpha alpha alpha alpha$", -1, -1, 3, true, true, vec![], vec![]);
        assert_eq!(parsed.decorations.len(), 1);
        assert_eq!(parsed.decorations[0].symbol, "α");
        assert_eq!(parsed.decorations[0].uuid, "alpha");
    }

    #[test]
    fn attachment() {
        let parsed = parse_document("$x^alpha x_alpha$", -1, -1, 3, true, true, vec![], vec![]);
        assert_eq!(parsed.decorations.len(), 2);
        assert_eq!(parsed.decorations[0].symbol, "α");
        assert_eq!(parsed.decorations[1].symbol, "α");
        let parsed = parse_document("$x^alpha$", -1, -1, 3, true, true, vec![], vec![]);
        assert_eq!(parsed.decorations[0].positions[0].start, 2);
        assert_eq!(parsed.decorations[0].uuid, "top-alpha");
        let parsed = parse_document("$x_alpha$", -1, -1, 3, true, true, vec![], vec![]);
        assert_eq!(parsed.decorations[0].uuid, "bottom-alpha");
    }
}
