grammar TypeCobol;

import CobolCodeElements;

// same starting rule as CodeElements
cobolCodeElements: codeElement* EOF;

someRule: 'Hello, world';