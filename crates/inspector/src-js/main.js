const TOKEN_COLORS = {
    StringLiteral: "#ff9ff3",
    NumberLiteral: "#feca57",
    BooleanLiteral: "#ff6b6b",
    NilLiteral: "#576574",

    Identifier: "#1dd1a1",
    Keyword: "#00d2d3",
    Operator: "#5f27cd",

    Comma: "#54a0ff",
    SemiColon: "#54a0ff",
    Colon: "#54a0ff",
    Assign: "#54a0ff",
    Dot: "#54a0ff",

    Vararg: "#54a0ff",

    Paren: "#54a0ff",
    SquareBrace: "#54a0ff",
    CurlyBrace: "#54a0ff",

    Comment: "#8395a7",
}

function createTokensRepr(input, tokens) {
    const reprElement = document.getElementById("repr");
    for (const child of reprElement.children)
        child.destroy();

    let currentElement = createRowElement();
    let currentRow = 0;
    let rowIndex = 0;

    const largestRowIndex = tokens[tokens.length - 1][1].end.row.toString().length;

    function createRowElement() {
        const element = document.createElement("div");
        element.classList.add("row");

        const rowElement = document.createElement("span");
        rowElement.

        rowIndex += 1;

        return element;
    }


    for (const [token, span] of tokens) {
        if (span.start.row !== currentRow) {
            const distance = span.end.row - currentRow;
            if (distance > 1)
                [].fill(distance - 1).forEach(() => {
                    createRowElement();
                });

            currentRow = span.end.row;

        }


        const element = document.createElement("span")
        const text = input.substring(span.start.offset, span.end.offset + 1);

        element.style.backgroundColor = TOKEN_COLORS[token.type];
        element.innerText = text;

        currentElement.append(element);
    }
}

async function init() {
    const pkg = await import("../pkg");
    pkg.init();

    const submitElement = document.getElementById("submit");
    const inputElement = document.getElementById("input");

    submitElement.addEventListener("click", () => {
        const input = inputElement.value;
        let tokens = pkg.lex(input);
        tokens = tokens.map(([token, span]) => {
            const type = typeof token === "string" ? token : Object.keys(token)[0];
            return [
                {
                    type,
                    value: Object.values(token)[0],
                },
                span
            ];
        });

        createTokensRepr(input, tokens);
    })
}

init().catch(console.error);
