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
        let parsed = parse_document(
            "$alpha alpha alpha alpha alpha$",
            -1,
            -1,
            3,
            true,
            true,
            vec![],
            vec![],
        );
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

        let parsed = parse_document("$x_alpha_alpha^alpha^alpha$", -1, -1, 3, true, true, vec![], vec![]);
        assert_eq!(parsed.decorations.len(), 3);
        let parsed = parse_document("$x_alpha_alpha^alpha^alpha$", -1, -1, 0, true, true, vec![], vec![]);
        assert_eq!(parsed.decorations.len(), 1);
    }

    #[test]
    fn test_edited_line() {
        let parsed = parse_document(
            "$zeta^2$\n#sym.arrow\n$(alpha)$",
            2,
            3,
            3,
            true,
            true,
            vec![],
            vec![],
        );
        assert_eq!(parsed.decorations.len(), 2);
        let parsed = parse_document(
            "\n\nnothing on this line\n$zeta^2$\n#sym.arrow\n$(alpha)$",
            0,
            0,
            3,
            true,
            true,
            vec![],
            vec![],
        );
        assert_eq!(parsed.decorations.len(), 0);
    }
    #[test]
    fn test_functions() {
        let parsed = parse_document(
            "$arrow(x)$",
            -1,
            -1,
            3,
            true,
            true,
            vec![],
            vec![],
        );
        assert_eq!(parsed.decorations.len(), 2);

        // Check that not too many decorations are added
        let parsed = parse_document(
            "$abs(x) x^abs(x) x_abs(x) arrow(abs(x))$",
            -1,
            -1,
            3,
            true,
            true,
            vec![],
            vec![],
        );
        assert_eq!(parsed.decorations.len(), 6);
        let parsed = parse_document(
            "$bb(\"hello\") cal(\"world!\") frak(!)$",
            -1,
            -1,
            3,
            true,
            true,
            vec![],
            vec![],
        );
        assert_eq!(parsed.decorations.len(), 3);
        let parsed = parse_document(
            "$dot(x) dot.double(x) tilde(x) norm(x) sqrt(2) sqrt(2^2)$",
            -1,
            -1,
            3,
            true,
            true,
            vec![],
            vec![],
        );
        assert_eq!(parsed.decorations.len(), 9);
    }
    #[test]
    fn test_field_access() {
        let parsed = parse_document(
            "$beta.alt$",
            -1,
            -1,
            3,
            true,
            true,
            vec![],
            vec![],
        );
        assert_eq!(parsed.decorations.len(), 1);
        assert_eq!(parsed.decorations[0].symbol, "ϐ");
        assert_eq!(parsed.decorations[0].uuid, "beta.alt");
        let parsed = parse_document(
            "$triangle.filled.b$",
            -1,
            -1,
            3,
            true,
            true,
            vec![],
            vec![],
        );
        assert_eq!(parsed.decorations.len(), 1);
        assert_eq!(parsed.decorations[0].symbol, "▼");
        assert_eq!(parsed.decorations[0].uuid, "triangle.filled.b");
    }
    #[test]
    fn test_text() {
        let parsed = parse_document(
            "$x^a x_a$",
            -1,
            -1,
            3,
            true,
            true,
            vec![],
            vec![],
        );
        assert_eq!(parsed.decorations.len(), 2);
        assert_eq!(parsed.decorations[0].symbol, "a");

        let parsed = parse_document(
            "$x^\"text\" x_\"text\"$",
            -1,
            -1,
            3,
            true,
            true,
            vec![],
            vec![],
        );
        assert_eq!(parsed.decorations.len(), 2);
        assert_eq!(parsed.decorations[0].symbol, "text");
    }
    #[test]
    fn test_linebreak() {
        let parsed = parse_document(
            "$x$ \\ \\ \\ x",
            -1,
            -1,
            3,
            true,
            true,
            vec![],
            vec![],
        );
        assert_eq!(parsed.decorations.len(), 1);
        assert_eq!(parsed.decorations[0].symbol, "⮰");
    }
    #[test]
    fn test_math_block() {
        let parsed = parse_document(
            "$x^(5+3-2)=6$",
            -1,
            -1,
            3,
            true,
            true,
            vec![],
            vec![],
        );
        assert_eq!(parsed.decorations.len(), 3);
        let parsed = parse_document(
            "$x^(alpha)$",
            -1,
            -1,
            3,
            true,
            true,
            vec![],
            vec![],
        );
        assert_eq!(parsed.decorations.len(), 2);
        let parsed = parse_document(
            "$x^(\"alpha\") x^(-\"alpha\") x^(-alpha)$",
            -1,
            -1,
            3,
            true,
            true,
            vec![],
            vec![],
        );
        assert_eq!(parsed.decorations.len(), 4);
    }
    #[test]
    fn test_shortands() {
        let parsed = parse_document(
            "$=> + - * |--> [ |]$",
            -1,
            -1,
            3,
            true,
            true,
            vec![],
            vec![],
        );
        assert_eq!(parsed.decorations.len(), 7);
    }
}
