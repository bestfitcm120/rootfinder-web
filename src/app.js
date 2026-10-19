/**
 * RootFinder Studio v3.0 - Numerical Methods Root-Finding Application
 * Bisection, Secant, and Newton-Raphson methods with full interactive visualization.
 */

(function () {
  'use strict';

  /* ==========================================================================
     1. MATHEMATICAL EXPRESSION PARSER & EVALUATOR
     ========================================================================== */

  // Additional trigonometric & hyperbolic math functions
  const MATH_EXT = {
    abs: Math.abs,
    sin: Math.sin,
    cos: Math.cos,
    tan: Math.tan,
    sec: (x) => {
      const c = Math.cos(x);
      if (Math.abs(c) < 1e-15) throw new Error("sec(x): division by zero");
      return 1 / c;
    },
    csc: (x) => {
      const s = Math.sin(x);
      if (Math.abs(s) < 1e-15) throw new Error("csc(x): division by zero");
      return 1 / s;
    },
    cosec: (x) => {
      const s = Math.sin(x);
      if (Math.abs(s) < 1e-15) throw new Error("cosec(x): division by zero");
      return 1 / s;
    },
    cot: (x) => {
      const s = Math.sin(x);
      if (Math.abs(s) < 1e-15) throw new Error("cot(x): division by zero");
      return Math.cos(x) / s;
    },
    asin: Math.asin,
    acos: Math.acos,
    atan: Math.atan,
    sinh: Math.sinh,
    cosh: Math.cosh,
    tanh: Math.tanh,
    sech: (x) => {
      const c = Math.cosh(x);
      if (Math.abs(c) < 1e-15) throw new Error("sech(x): division by zero");
      return 1 / c;
    },
    csch: (x) => {
      const s = Math.sinh(x);
      if (Math.abs(s) < 1e-15) throw new Error("csch(x): division by zero");
      return 1 / s;
    },
    coth: (x) => {
      const t = Math.tanh(x);
      if (Math.abs(t) < 1e-15) throw new Error("coth(x): division by zero");
      return 1 / t;
    },
    asinh: Math.asinh,
    acosh: Math.acosh,
    atanh: Math.atanh,
    exp: Math.exp,
    log: Math.log,
    ln: Math.log,
    log10: Math.log10,
    log2: Math.log2,
    sqrt: Math.sqrt,
    cbrt: Math.cbrt,
    pi: Math.PI,
    e: Math.E
  };

  /**
   * Tokenizer & Parser for natural mathematical expressions.
   * Expands implicit multiplication: 2x -> 2*x, 3sin(x) -> 3*sin(x), (x+1)(x-1) -> (x+1)*(x-1)
   * Converts ^ to exponentiation.
   */
  class MathParser {
    static sanitize(expr) {
      if (!expr || typeof expr !== 'string') return '';
      let s = expr.trim();
      s = s.replace(/×/g, '*').replace(/÷/g, '/');
      return s;
    }

    /**
     * Preprocesses math expression string to inject multiplication and standardize operators.
     */
    static preprocess(rawExpr) {
      let s = MathParser.sanitize(rawExpr);
      if (!s) throw new Error("Empty expression");

      // Replace ^ with ** for power
      // Implicit multiplication replacements:
      // 1. ) followed by ( or variable/number: ) ( -> )*( , )(x -> )*(x
      s = s.replace(/\)\s*(?=[(\w])/g, ')*');
      // 2. Number followed by letter or function: 2x -> 2*x, 3sin -> 3*sin, 4.5e -> 4.5*e (unless scientific notation like 1e5)
      s = s.replace(/(?<![a-zA-Z_])(\d+\.?\d*)\s*(?!(?:[eE][+-]?\d+))([a-zA-Z_]\w*)/g, '$1*$2');
      // 3. Number followed by (: 2(x+1) -> 2*(x+1)
      s = s.replace(/(?<![a-zA-Z_])(\d+\.?\d*)\s*(?=\()/g, '$1*');
      // 4. Variable x followed by letter, function, or (: x(x+1) -> x*(x+1), x sin(x) -> x*sin(x)
      s = s.replace(/\bx\b\s*(?=[a-zA-Z_]|\()/g, 'x*');
      // 5. Clean duplicate multiplication operators that might have been created
      s = s.replace(/\*{2,}/g, '^'); // preserve intentional power or collapse
      s = s.replace(/\*+/g, '*');
      s = s.replace(/\*\^/g, '^');
      return s;
    }

    /**
     * Compile expression to an AST or evaluatable function f(x)
     */
    static compile(exprStr) {
      const cleanExpr = MathParser.preprocess(exprStr);

      // Tokenize
      const tokens = MathParser.tokenize(cleanExpr);
      const rpn = MathParser.toRPN(tokens);

      return function evaluate(x) {
        return MathParser.evaluateRPN(rpn, x);
      };
    }

    static tokenize(str) {
      const tokens = [];
      let i = 0;
      const len = str.length;

      while (i < len) {
        const c = str[i];

        if (/\s/.test(c)) {
          i++;
          continue;
        }

        // Numbers: digits and decimals, including scientific notation 1e-4
        if (/\d/.test(c) || (c === '.' && /\d/.test(str[i + 1] || ''))) {
          let numStr = '';
          while (i < len && (/[\d.]/.test(str[i]) || ((str[i] === 'e' || str[i] === 'E') && /[+-]?\d/.test(str.substr(i + 1, 2))))) {
            if (str[i] === 'e' || str[i] === 'E') {
              numStr += str[i++];
              if (str[i] === '+' || str[i] === '-') numStr += str[i++];
            } else {
              numStr += str[i++];
            }
          }
          tokens.push({ type: 'NUM', value: parseFloat(numStr) });
          continue;
        }

        // Identifiers: variables (x), constants (pi, e), or function names (sin, cos)
        if (/[a-zA-Z_]/.test(c)) {
          let id = '';
          while (i < len && /[a-zA-Z0-9_]/.test(str[i])) {
            id += str[i++];
          }
          const lower = id.toLowerCase();
          if (lower === 'x') {
            tokens.push({ type: 'VAR', value: 'x' });
          } else if (lower === 'pi') {
            tokens.push({ type: 'NUM', value: Math.PI });
          } else if (lower === 'e' && tokens.length > 0 && tokens[tokens.length - 1].type === 'NUM') {
            // Already handled in number scanner, but just in case
            tokens.push({ type: 'NUM', value: Math.E });
          } else if (lower === 'e') {
            tokens.push({ type: 'NUM', value: Math.E });
          } else if (MATH_EXT[lower]) {
            tokens.push({ type: 'FUNC', value: lower });
          } else {
            throw new Error(`Unknown function or symbol: "${id}"`);
          }
          continue;
        }

        // Operators & Parentheses
        if ('+-*/^(),'.includes(c)) {
          // Detect unary minus/plus: at start, or after an operator or '('
          if (c === '-' || c === '+') {
            const prev = tokens[tokens.length - 1];
            if (!prev || prev.type === 'OP' || prev.type === 'LPAREN' || prev.type === 'COMMA') {
              tokens.push({ type: 'UNARY', value: c === '-' ? 'neg' : 'pos' });
              i++;
              continue;
            }
          }

          if (c === '(') tokens.push({ type: 'LPAREN' });
          else if (c === ')') tokens.push({ type: 'RPAREN' });
          else if (c === ',') tokens.push({ type: 'COMMA' });
          else tokens.push({ type: 'OP', value: c });
          i++;
          continue;
        }

        throw new Error(`Unexpected character: "${c}"`);
      }
      return tokens;
    }

    // Shunting-yard algorithm: Infix -> Reverse Polish Notation (RPN)
    static toRPN(tokens) {
      const output = [];
      const opStack = [];

      const precedence = {
        '+': 1,
        '-': 1,
        '*': 2,
        '/': 2,
        'neg': 3,
        'pos': 3,
        '^': 4
      };

      const rightAssoc = {
        '^': true,
        'neg': true,
        'pos': true
      };

      for (let i = 0; i < tokens.length; i++) {
        const token = tokens[i];

        if (token.type === 'NUM' || token.type === 'VAR') {
          output.push(token);
        } else if (token.type === 'FUNC') {
          opStack.push(token);
        } else if (token.type === 'COMMA') {
          while (opStack.length > 0 && opStack[opStack.length - 1].type !== 'LPAREN') {
            output.push(opStack.pop());
          }
          if (opStack.length === 0) throw new Error("Mismatched comma or parentheses");
        } else if (token.type === 'OP' || token.type === 'UNARY') {
          const o1 = token.value;
          while (opStack.length > 0) {
            const top = opStack[opStack.length - 1];
            if (top.type === 'OP' || top.type === 'UNARY') {
              const o2 = top.value;
              const p1 = precedence[o1];
              const p2 = precedence[o2];
              if ((!rightAssoc[o1] && p1 <= p2) || (rightAssoc[o1] && p1 < p2)) {
                output.push(opStack.pop());
                continue;
              }
            }
            break;
          }
          opStack.push(token);
        } else if (token.type === 'LPAREN') {
          opStack.push(token);
        } else if (token.type === 'RPAREN') {
          let foundLparen = false;
          while (opStack.length > 0) {
            const top = opStack.pop();
            if (top.type === 'LPAREN') {
              foundLparen = true;
              break;
            }
            output.push(top);
          }
          if (!foundLparen) throw new Error("Mismatched parentheses");
          if (opStack.length > 0 && opStack[opStack.length - 1].type === 'FUNC') {
            output.push(opStack.pop());
          }
        }
      }

      while (opStack.length > 0) {
        const top = opStack.pop();
        if (top.type === 'LPAREN' || top.type === 'RPAREN') {
          throw new Error("Mismatched parentheses");
        }
        output.push(top);
      }

      return output;
    }

    static evaluateRPN(rpn, x) {
      const stack = [];

      for (let i = 0; i < rpn.length; i++) {
        const token = rpn[i];

        if (token.type === 'NUM') {
          stack.push(token.value);
        } else if (token.type === 'VAR') {
          stack.push(x);
        } else if (token.type === 'UNARY') {
          if (stack.length < 1) throw new Error("Invalid unary expression");
          const v = stack.pop();
          stack.push(token.value === 'neg' ? -v : v);
        } else if (token.type === 'OP') {
          if (stack.length < 2) throw new Error("Invalid binary expression");
          const b = stack.pop();
          const a = stack.pop();
          switch (token.value) {
            case '+': stack.push(a + b); break;
            case '-': stack.push(a - b); break;
            case '*': stack.push(a * b); break;
            case '/':
              if (Math.abs(b) < 1e-15) stack.push(a >= 0 ? Infinity : -Infinity);
              else stack.push(a / b);
              break;
            case '^': stack.push(Math.pow(a, b)); break;
            default: throw new Error(`Unknown operator: ${token.value}`);
          }
        } else if (token.type === 'FUNC') {
          if (stack.length < 1) throw new Error(`Function ${token.value} requires an argument`);
          const arg = stack.pop();
          const fn = MATH_EXT[token.value];
          if (!fn) throw new Error(`Unknown function: ${token.value}`);
          stack.push(fn(arg));
        }
      }

      if (stack.length !== 1) throw new Error("Evaluation error: invalid stack length");
      const res = stack[0];
      return typeof res === 'number' ? res : NaN;
    }
  }

  /* ==========================================================================
     2. NUMERICAL UTILITIES & DOMAIN SCANNER
     ========================================================================== */

  /**
   * High-accuracy central difference derivative f'(x)
   */
  function numericalDerivative(f, x, h = 1e-7) {
    const v1 = f(x + h);
    const v2 = f(x - h);
    return (v1 - v2) / (2 * h);
  }

  /**
   * Format numbers cleanly without -0.0
   */
  function cleanNum(val, threshold = 1e-11) {
    if (Math.abs(val) < threshold) return 0;
    return val;
  }

  function fmtVal(val, decimals = 6) {
    if (val === null || val === undefined || isNaN(val)) return '—';
    if (!isFinite(val)) return val > 0 ? '+∞' : '-∞';
    const c = cleanNum(val);
    if (Math.abs(c) >= 1e6 || (Math.abs(c) < 1e-4 && c !== 0)) {
      return c.toExponential(4);
    }
    return c.toFixed(decimals).replace(/\.?0+$/, (match) => {
      // keep at least 2 decimal digits unless zero
      return match.length > 4 ? '' : match;
    });
  }

  function fmtError(val) {
    if (val === null || val === undefined || isNaN(val)) return '—';
    if (!isFinite(val)) return '∞';
    const c = cleanNum(val);
    if (c === 0) return '0.0';
    if (c >= 0.001 && c < 1000) return c.toFixed(6);
    return c.toExponential(4);
  }

  function fmtRelError(val) {
    if (val === null || val === undefined || isNaN(val)) return '—';
    if (!isFinite(val)) return '∞';
    const c = cleanNum(val);
    const pct = c * 100;
    if (c === 0) return '0.0% (0.0)';
    if (pct >= 0.001 && pct < 1000) {
      return `${pct.toFixed(4)}% (${c.toFixed(6)})`;
    }
    return `${pct.toExponential(3)}% (${c.toExponential(3)})`;
  }

  /**
   * Parses tolerance text (e.g. "0.0001%", "1e-5", "0.001")
   */
  function parseTolerance(str) {
    if (!str || typeof str !== 'string') return 1e-6;
    const s = str.trim();
    if (s.endsWith('%')) {
      const num = parseFloat(s.slice(0, -1));
      return isNaN(num) || num <= 0 ? 1e-6 : num / 100;
    }
    const num = parseFloat(s);
    return isNaN(num) || num <= 0 ? 1e-6 : num;
  }

  /**
   * Automated Domain Scanner to detect root brackets via Bolzano sign-change
   */
  function findBrackets(f, xMin = -10, xMax = 10, steps = 200) {
    if (xMin >= xMax) [xMin, xMax] = [xMax, xMin];
    const dx = (xMax - xMin) / steps;
    const brackets = [];

    let prevX = null;
    let prevF = null;

    for (let i = 0; i <= steps; i++) {
      const x = xMin + i * dx;
      let val;
      try {
        val = f(x);
        if (!isFinite(val) || isNaN(val)) {
          prevX = null;
          prevF = null;
          continue;
        }
      } catch (e) {
        prevX = null;
        prevF = null;
        continue;
      }

      if (prevX !== null && prevF !== null) {
        if (Math.abs(prevF) < 1e-13) {
          brackets.push({ a: Number(prevX.toFixed(4)), b: Number(prevX.toFixed(4)), exact: true });
        } else if (prevF * val < 0) {
          brackets.push({ a: Number(prevX.toFixed(4)), b: Number(x.toFixed(4)), exact: false });
        }
      }

      prevX = x;
      prevF = val;
    }

    return brackets;
  }

  /* ==========================================================================
     3. NUMERICAL ROOT-FINDING SOLVERS
     ========================================================================== */

  /**
   * Bisection Method Solver
   */
  function solveBisection(f, a, b, tol, maxIter = 1000) {
    const rows = [];
    if (a > b) [a, b] = [b, a];

    let fa = f(a);
    let fb = f(b);

    if (a === b || Math.abs(fa) < 1e-15) {
      rows.push({
        iteration: 1,
        a, b, c: a,
        fVal: fa,
        ea: 0,
        relEa: 0,
        desc: `Initial endpoint a=${a} is already a root.`
      });
      return { root: a, iterations: 1, rows, converged: true, errorMsg: null };
    }

    if (Math.abs(fb) < 1e-15) {
      rows.push({
        iteration: 1,
        a, b, c: b,
        fVal: fb,
        ea: 0,
        relEa: 0,
        desc: `Initial endpoint b=${b} is already a root.`
      });
      return { root: b, iterations: 1, rows, converged: true, errorMsg: null };
    }

    if (fa * fb > 0) {
      return {
        root: null,
        iterations: 0,
        rows: [],
        converged: false,
        errorMsg: `Bolzano theorem violation: f(${a})=${fmtVal(fa)} and f(${b})=${fmtVal(fb)} have the SAME sign! A sign change is required.`
      };
    }

    let prevC = null;
    let currA = a;
    let currB = b;

    for (let i = 1; i <= maxIter; i++) {
      const c = (currA + currB) / 2;
      const fc = f(c);

      const ea = prevC !== null ? Math.abs(c - prevC) : null;
      const relEa = (ea !== null && Math.abs(c) > 1e-15) ? (ea / Math.abs(c)) : (ea !== null ? Infinity : null);

      let stepDesc = '';
      if (i === 1) {
        stepDesc = `Midpoint c₁ = (${fmtVal(currA)} + ${fmtVal(currB)}) / 2 = ${fmtVal(c)}, f(c₁) = ${fmtVal(fc)}`;
      } else {
        stepDesc = `Midpoint c${i} = ${fmtVal(c)}, Δx = ${fmtVal(ea)}, ε_a = ${fmtRelError(relEa)}`;
      }

      rows.push({
        iteration: i,
        a: currA,
        b: currB,
        c,
        fVal: fc,
        ea,
        relEa,
        desc: stepDesc
      });

      // Stopping condition: f(c) near 0 or relative approximate error <= tol
      if (Math.abs(fc) < 1e-15 || (relEa !== null && relEa <= tol)) {
        return { root: cleanNum(c), iterations: i, rows, converged: true, errorMsg: null };
      }

      const fCurrA = f(currA);
      if (fCurrA * fc < 0) {
        currB = c; // root is in left half
      } else {
        currA = c; // root is in right half
      }
      prevC = c;
    }

    return { root: cleanNum(rows[rows.length - 1].c), iterations: maxIter, rows, converged: false, errorMsg: "Reached maximum iterations without meeting tolerance." };
  }

  /**
   * Secant Method Solver
   */
  function solveSecant(f, x0, x1, tol, maxIter = 1000) {
    const rows = [];
    let p0 = x0;
    let p1 = x1;
    let f0 = f(p0);
    let f1 = f(p1);

    if (p0 === p1) {
      return {
        root: null,
        iterations: 0,
        rows: [],
        converged: false,
        errorMsg: "Initial guesses x₀ and x₁ must be distinct numbers."
      };
    }

    for (let i = 1; i <= maxIter; i++) {
      const denom = f1 - f0;
      if (Math.abs(denom) < 1e-15) {
        return {
          root: cleanNum(p1),
          iterations: rows.length,
          rows,
          converged: false,
          errorMsg: `Denominator f(x_${i}) - f(x_${i - 1}) near zero (nearly horizontal chord). Stopped to prevent division by zero.`
        };
      }

      // Secant formula: p2 = p1 - f1 * (p1 - p0) / (f1 - f0)
      const p2 = p1 - f1 * (p1 - p0) / denom;
      const f2 = f(p2);
      const ea = Math.abs(p2 - p1);
      const relEa = Math.abs(p2) > 1e-15 ? (ea / Math.abs(p2)) : Infinity;

      const stepDesc = `Chord between (${fmtVal(p0)}, ${fmtVal(f0)}) & (${fmtVal(p1)}, ${fmtVal(f1)}) -> x_${i + 1} = ${fmtVal(p2)}, ε_a = ${fmtRelError(relEa)}`;

      rows.push({
        iteration: i,
        p0,
        p1,
        p2,
        estimate: p2,
        f0,
        f1,
        fVal: f2,
        ea,
        relEa,
        desc: stepDesc
      });

      if (Math.abs(f2) < 1e-15 || relEa <= tol) {
        return { root: cleanNum(p2), iterations: i, rows, converged: true, errorMsg: null };
      }

      p0 = p1;
      p1 = p2;
      f0 = f1;
      f1 = f2;
    }

    return { root: cleanNum(rows[rows.length - 1].p2), iterations: maxIter, rows, converged: false, errorMsg: "Reached maximum iterations without meeting tolerance." };
  }

  /**
   * Newton-Raphson Method Solver
   */
  function solveNewton(f, x0, tol, maxIter = 1000) {
    const rows = [];
    let curr = x0;
    let prev = null;

    for (let i = 1; i <= maxIter; i++) {
      const fc = f(curr);
      const dfc = numericalDerivative(f, curr);

      const ea = prev !== null ? Math.abs(curr - prev) : null;
      const relEa = (ea !== null && Math.abs(curr) > 1e-15) ? (ea / Math.abs(curr)) : (ea !== null ? Infinity : null);

      const stepDesc = `Tangent at x_${i} = ${fmtVal(curr)}: slope f'=${fmtVal(dfc)}, f(x)=${fmtVal(fc)}, ε_a=${fmtRelError(relEa)}`;

      rows.push({
        iteration: i,
        xn: curr,
        estimate: curr,
        fVal: fc,
        dfVal: dfc,
        ea,
        relEa,
        desc: stepDesc
      });

      if (Math.abs(fc) < 1e-15 || (relEa !== null && relEa <= tol)) {
        return { root: cleanNum(curr), iterations: i, rows, converged: true, errorMsg: null };
      }

      if (Math.abs(dfc) < 1e-14) {
        return {
          root: cleanNum(curr),
          iterations: i,
          rows,
          converged: false,
          errorMsg: `Derivative f'(${fmtVal(curr)}) is near zero (horizontal tangent line). Cannot project to x-axis.`
        };
      }

      prev = curr;
      curr = curr - fc / dfc;
    }

    return { root: cleanNum(rows[rows.length - 1].xn), iterations: maxIter, rows, converged: false, errorMsg: "Reached maximum iterations without meeting tolerance." };
  }

  /* ==========================================================================
     4. APPLICATION STATE & DOM REFERENCES
     ========================================================================== */

  const state = {
    method: 'bisection',
    exprString: 'x^3 - 7x + 1',
    f: null,
    compiledExpr: null,
    tol: 1e-6,
    maxIter: 100,
    trueRoot: null,
    inputs: {
      bisA: 0,
      bisB: 1,
      secX0: 0,
      secX1: 1,
      newtonX0: 0.5
    },
    currentResult: null,
    shootoutResults: null,
    // Step-by-step playback state
    playback: {
      currentStep: 1,
      isPlaying: false,
      timer: null,
      speed: 600
    },
    // Interactive step animation state
    stepAnimation: {
      enabled: true,
      active: false,
      t: 1.0,
      fromStep: 1,
      toStep: 1,
      startTime: 0,
      duration: 450,
      reqId: null
    },
    // Viewport transform for geometric canvas (drag & zoom)
    view: {
      tool: 'pan', // 'pan' | 'select-zoom'
      autoZoom: true,
      xMin: -1,
      xMax: 2,
      yMin: -6,
      yMax: 4,
      isDragging: false,
      dragStartX: 0,
      dragStartY: 0,
      origXMin: -1,
      origXMax: 2,
      origYMin: -6,
      origYMax: 4,
      zoomAnimId: null
    },
    // Marquee / Box Selection state for Select Zoom
    selection: {
      active: false,
      startX: 0,
      startY: 0,
      currentX: 0,
      currentY: 0
    }
  };

  // DOM Elements
  const el = {
    form: document.getElementById('solver-form'),
    methodTabs: document.querySelectorAll('.method-tab'),
    inputFunc: document.getElementById('input-func'),
    funcPreview: document.getElementById('func-preview'),
    btnScanRoots: document.getElementById('btn-scan-roots'),
    inputsBisection: document.getElementById('inputs-bisection'),
    inputsSecant: document.getElementById('inputs-secant'),
    inputsNewton: document.getElementById('inputs-newton'),
    inputBisA: document.getElementById('input-bis-a'),
    inputBisB: document.getElementById('input-bis-b'),
    bolzanoBadge: document.getElementById('bolzano-check-badge'),
    inputSecX0: document.getElementById('input-sec-x0'),
    inputSecX1: document.getElementById('input-sec-x1'),
    inputNewtonX0: document.getElementById('input-newton-x0'),
    inputTol: document.getElementById('input-tol'),
    inputMaxIter: document.getElementById('input-max-iter'),
    inputTrueRoot: document.getElementById('input-true-root'),
    solverAlert: document.getElementById('solver-alert'),
    btnCalculate: document.getElementById('btn-calculate'),
    btnCompareAll: document.getElementById('btn-compare-all'),
    presetSelect: document.getElementById('preset-select'),
    // Scanner
    scannerCard: document.getElementById('scanner-results-card'),
    scannerSummaryText: document.getElementById('scanner-summary-text'),
    bracketPillsList: document.getElementById('bracket-pills-list'),
    btnCloseScanner: document.getElementById('btn-close-scanner'),
    // Metrics
    metricRoot: document.getElementById('metric-root'),
    metricFVal: document.getElementById('metric-f-val'),
    metricIters: document.getElementById('metric-iters'),
    metricMethodName: document.getElementById('metric-method-name'),
    metricRelEa: document.getElementById('metric-rel-ea'),
    metricAbsEa: document.getElementById('metric-abs-ea'),
    metricExactErr: document.getElementById('metric-exact-err'),
    metricExactRel: document.getElementById('metric-exact-rel'),
    // Workspace tabs
    viewTabs: document.querySelectorAll('.nav-tab'),
    viewPanels: {
      vis: document.getElementById('view-vis'),
      table: document.getElementById('view-table'),
      shootout: document.getElementById('view-shootout')
    },
    tableActionsToolbar: document.getElementById('table-actions-toolbar'),
    tableRowCount: document.getElementById('table-row-count'),
    // Playback
    btnStepFirst: document.getElementById('btn-step-first'),
    btnStepPrev: document.getElementById('btn-step-prev'),
    btnPlayPause: document.getElementById('btn-play-pause'),
    iconPlay: document.getElementById('icon-play'),
    iconPause: document.getElementById('icon-pause'),
    btnStepNext: document.getElementById('btn-step-next'),
    btnStepLast: document.getElementById('btn-step-last'),
    stepSlider: document.getElementById('step-slider'),
    stepBadge: document.getElementById('step-badge'),
    playbackSpeed: document.getElementById('playback-speed'),
    toggleAnimSteps: document.getElementById('toggle-anim-steps'),
    toggleAutoZoom: document.getElementById('toggle-auto-zoom'),
    btnResetView: document.getElementById('btn-reset-view'),
    btnSpanScreen: document.getElementById('btn-span-screen'),
    iconSpanExpand: document.getElementById('icon-span-expand'),
    iconSpanCollapse: document.getElementById('icon-span-collapse'),
    appLayout: document.querySelector('.app-layout'),
    // Fullscreen Graph Controls
    geomCanvasPanel: document.getElementById('geom-canvas-panel'),
    btnGeomFullscreen: document.getElementById('btn-geom-fullscreen'),
    iconEnterFullscreen: document.getElementById('icon-enter-fullscreen'),
    iconExitFullscreen: document.getElementById('icon-exit-fullscreen'),
    fullscreenHud: document.getElementById('fullscreen-hud'),
    fsBtnStepFirst: document.getElementById('fs-btn-step-first'),
    fsBtnStepPrev: document.getElementById('fs-btn-step-prev'),
    fsBtnPlayPause: document.getElementById('fs-btn-play-pause'),
    fsIconPlay: document.getElementById('fs-icon-play'),
    fsIconPause: document.getElementById('fs-icon-pause'),
    fsBtnStepNext: document.getElementById('fs-btn-step-next'),
    fsBtnStepLast: document.getElementById('fs-btn-step-last'),
    fsStepSlider: document.getElementById('fs-step-slider'),
    fsStepBadge: document.getElementById('fs-step-badge'),
    fsPlaybackSpeed: document.getElementById('fs-playback-speed'),
    fsToggleAnimSteps: document.getElementById('fs-toggle-anim-steps'),
    fsToggleAutoZoom: document.getElementById('fs-toggle-auto-zoom'),
    btnFsExit: document.getElementById('btn-fs-exit'),
    // Canvas Toolbar & Tools
    toolPan: document.getElementById('tool-pan'),
    toolSelectZoom: document.getElementById('tool-select-zoom'),
    btnZoomIn: document.getElementById('btn-zoom-in'),
    btnZoomOut: document.getElementById('btn-zoom-out'),
    canvasSelectionBox: document.getElementById('canvas-selection-box'),
    canvasSelectionLabel: document.getElementById('canvas-selection-label'),
    canvasHint: document.getElementById('canvas-hint'),
    // Canvases
    geomCanvas: document.getElementById('geom-canvas'),
    geomCanvasWrapper: document.getElementById('geom-canvas-wrapper'),
    errorCanvas: document.getElementById('error-canvas'),
    shootoutCanvas: document.getElementById('shootout-canvas'),
    canvasTooltip: document.getElementById('canvas-tooltip'),
    geomStepLabel: document.getElementById('geom-step-label'),
    geomLegend: document.getElementById('geom-legend'),
    stepDetailBox: document.getElementById('step-detail-box'),
    stepNumberTag: document.getElementById('step-number-tag'),
    stepActionDesc: document.getElementById('step-action-desc'),
    stepDetailMath: document.getElementById('step-detail-math'),
    // Tables
    iterationTableHead: document.getElementById('iteration-table-head'),
    iterationTableBody: document.getElementById('iteration-table-body'),
    shootoutTableBody: document.getElementById('shootout-table-body'),
    btnCopyTable: document.getElementById('btn-copy-table'),
    btnExportCsv: document.getElementById('btn-export-csv'),
    // Theory Modal
    theoryDialog: document.getElementById('theory-dialog'),
    btnOpenTheory: document.getElementById('btn-open-theory'),
    btnCloseTheory: document.getElementById('btn-close-theory'),
    btnTheoryDone: document.getElementById('btn-theory-done'),
    theoryTabs: document.querySelectorAll('.theory-tab-btn'),
    theoryContents: document.querySelectorAll('.theory-content'),
    // Theme toggle buttons
    themeButtons: document.querySelectorAll('.theme-btn')
  };

  /* ==========================================================================
     5. THEME SWITCHER CONTROLLER (Modern Web Guidance)
     ========================================================================== */

  function initTheme() {
    const meta = document.querySelector('meta[name="color-scheme"]');
    const stored = localStorage.getItem("theme-preference") || "system";

    function applyTheme(pref) {
      if (pref === "dark") {
        if (meta) meta.content = "dark";
        document.documentElement.setAttribute('data-theme', 'dark');
      } else if (pref === "light") {
        if (meta) meta.content = "light";
        document.documentElement.setAttribute('data-theme', 'light');
      } else {
        if (meta) meta.content = "light dark";
        document.documentElement.setAttribute('data-theme', 'system');
      }

      el.themeButtons.forEach(btn => {
        const val = btn.dataset.themeVal;
        btn.classList.toggle('active', val === pref);
      });

      // Redraw canvases on theme switch to ensure crisp contrast
      renderCanvases();
    }

    el.themeButtons.forEach(btn => {
      btn.addEventListener('click', () => {
        const pref = btn.dataset.themeVal;
        if (pref === "system") {
          localStorage.removeItem("theme-preference");
        } else {
          localStorage.setItem("theme-preference", pref);
        }
        applyTheme(pref);
      });
    });

    window.addEventListener('storage', (e) => {
      if (e.key === "theme-preference") {
        applyTheme(e.newValue || "system");
      }
    });

    applyTheme(stored);
  }

  /* ==========================================================================
     6. CANVAS RENDERING ENGINE (High-DPI 2-Panel & Shootout)
     ========================================================================== */

  function getThemeColors() {
    const isDark = window.matchMedia('(prefers-color-scheme: dark)').matches ||
      document.documentElement.getAttribute('data-theme') === 'dark';

    return {
      bg: isDark ? '#111827' : '#ffffff',
      grid: isDark ? 'rgba(255, 255, 255, 0.08)' : 'rgba(0, 0, 0, 0.07)',
      axis: isDark ? '#64748b' : '#94a3b8',
      text: isDark ? '#94a3b8' : '#64748b',
      textMain: isDark ? '#f8fafc' : '#0f172a',
      curve: isDark ? '#38bdf8' : '#0284c7',
      primary: isDark ? '#3b82f6' : '#2563eb',
      accent: isDark ? '#a855f7' : '#7c3aed',
      danger: isDark ? '#f87171' : '#dc2626',
      success: isDark ? '#4ade80' : '#16a34a',
      warning: isDark ? '#fbbf24' : '#d97706',
      bracketFill: isDark ? 'rgba(56, 189, 248, 0.15)' : 'rgba(2, 132, 199, 0.12)'
    };
  }

  function resizeCanvas(canvas) {
    const rect = canvas.getBoundingClientRect();
    const dpr = window.devicePixelRatio || 1;
    if (canvas.width !== Math.floor(rect.width * dpr) || canvas.height !== Math.floor(rect.height * dpr)) {
      canvas.width = Math.floor(rect.width * dpr);
      canvas.height = Math.floor(rect.height * dpr);
    }
    const ctx = canvas.getContext('2d');
    ctx.resetTransform();
    ctx.scale(dpr, dpr);
    return { width: rect.width, height: rect.height, ctx };
  }

  /**
   * Panel 1: Geometric Visualizer (Curve f(x), brackets, chord lines, tangents)
   */
  function renderGeomCanvas() {
    const canvas = el.geomCanvas;
    if (!canvas) return;
    const { width, height, ctx } = resizeCanvas(canvas);
    const colors = getThemeColors();

    ctx.clearRect(0, 0, width, height);

    // Coordinate conversion utilities
    const { xMin, xMax, yMin, yMax } = state.view;
    const toScreenX = (x) => ((x - xMin) / (xMax - xMin)) * width;
    const toScreenY = (y) => height - ((y - yMin) / (yMax - yMin)) * height;
    const fromScreenX = (sx) => xMin + (sx / width) * (xMax - xMin);
    const fromScreenY = (sy) => yMin + ((height - sy) / height) * (yMax - yMin);

    // 1. Draw Grid Lines
    ctx.lineWidth = 1;
    ctx.strokeStyle = colors.grid;

    // Pick nice step sizes for ticks
    const xSpan = xMax - xMin;
    const ySpan = yMax - yMin;
    const xStep = Math.pow(10, Math.floor(Math.log10(xSpan))) * (xSpan / Math.pow(10, Math.floor(Math.log10(xSpan))) > 5 ? 1 : 0.5);
    const yStep = Math.pow(10, Math.floor(Math.log10(ySpan))) * (ySpan / Math.pow(10, Math.floor(Math.log10(ySpan))) > 5 ? 1 : 0.5);

    ctx.fillStyle = colors.text;
    ctx.font = '10px monospace';

    const formatTick = (val, step) => {
      if (Math.abs(val) < 1e-12) return '0';
      const dec = Math.max(0, Math.min(8, -Math.floor(Math.log10(step)) + 1));
      const rounded = parseFloat(val.toFixed(dec));
      if (Math.abs(rounded) >= 1e5 || (Math.abs(rounded) < 1e-4 && rounded !== 0)) {
        return rounded.toExponential(2);
      }
      return cleanNum(rounded, 1e-12).toString();
    };

    // Vertical grid & X-axis ticks
    const firstX = Math.floor(xMin / xStep) * xStep;
    for (let x = firstX; x <= xMax; x += xStep) {
      const sx = toScreenX(x);
      ctx.beginPath();
      ctx.moveTo(sx, 0);
      ctx.lineTo(sx, height);
      ctx.stroke();

      // Tick label
      const syZero = Math.min(Math.max(toScreenY(0) + 12, 14), height - 4);
      ctx.fillText(formatTick(x, xStep), sx + 2, syZero);
    }

    // Horizontal grid & Y-axis ticks
    const firstY = Math.floor(yMin / yStep) * yStep;
    for (let y = firstY; y <= yMax; y += yStep) {
      const sy = toScreenY(y);
      ctx.beginPath();
      ctx.moveTo(0, sy);
      ctx.lineTo(width, sy);
      ctx.stroke();

      // Tick label
      const sxZero = Math.min(Math.max(toScreenX(0) + 4, 4), width - 35);
      if (Math.abs(y) > 1e-9) {
        ctx.fillText(formatTick(y, yStep), sxZero, sy - 2);
      }
    }

    // 2. Axes (X=0 and Y=0)
    ctx.lineWidth = 1.5;
    ctx.strokeStyle = colors.axis;

    const zeroY = toScreenY(0);
    if (zeroY >= 0 && zeroY <= height) {
      ctx.beginPath();
      ctx.moveTo(0, zeroY);
      ctx.lineTo(width, zeroY);
      ctx.stroke();
    }

    const zeroX = toScreenX(0);
    if (zeroX >= 0 && zeroX <= width) {
      ctx.beginPath();
      ctx.moveTo(zeroX, 0);
      ctx.lineTo(zeroX, height);
      ctx.stroke();
    }

    // 3. Draw Function Curve y = f(x)
    if (state.f) {
      ctx.lineWidth = 2.2;
      ctx.strokeStyle = colors.curve;
      ctx.beginPath();

      const numSamples = Math.min(width * 2, 1000);
      let started = false;

      for (let i = 0; i <= numSamples; i++) {
        const sx = (i / numSamples) * width;
        const x = fromScreenX(sx);
        let y;
        try {
          y = state.f(x);
        } catch (e) {
          started = false;
          continue;
        }

        if (!isFinite(y) || isNaN(y) || y < yMin - ySpan * 2 || y > yMax + ySpan * 2) {
          started = false;
          continue;
        }

        const sy = toScreenY(y);
        if (!started) {
          ctx.moveTo(sx, sy);
          started = true;
        } else {
          ctx.lineTo(sx, sy);
        }
      }
      ctx.stroke();
    }

    // 4. Method Specific Geometric Constructions
    const result = state.currentResult;
    if (result && result.rows.length > 0) {
      const stepIdx = Math.min(state.playback.currentStep - 1, result.rows.length - 1);
      const row = result.rows[stepIdx];
      const prevRow = stepIdx > 0 ? result.rows[stepIdx - 1] : null;
      const animT = state.stepAnimation.t;

      if (state.method === 'bisection') {
        renderBisectionConstruction(ctx, row, prevRow, toScreenX, toScreenY, colors, height, animT);
      } else if (state.method === 'secant') {
        renderSecantConstruction(ctx, row, toScreenX, toScreenY, colors, animT);
      } else if (state.method === 'newton') {
        renderNewtonConstruction(ctx, row, toScreenX, toScreenY, colors, animT);
      }

      // 5. Draw all previous approximations as small dots on x-axis
      ctx.fillStyle = colors.accent;
      for (let i = 0; i <= stepIdx; i++) {
        const r = result.rows[i];
        const est = r.c !== undefined ? r.c : (r.p2 !== undefined ? r.p2 : r.xn);
        const estX = toScreenX(est);
        const estY = toScreenY(0);

        ctx.beginPath();
        ctx.arc(estX, estY, 3.5, 0, Math.PI * 2);
        ctx.fill();
      }
    }

    // 6. Draw Known True Root marker if provided
    if (state.trueRoot !== null && isFinite(state.trueRoot)) {
      const rx = toScreenX(state.trueRoot);
      const ry = toScreenY(0);
      ctx.fillStyle = colors.success;
      ctx.beginPath();
      ctx.arc(rx, ry, 6, 0, Math.PI * 2);
      ctx.fill();
      ctx.strokeStyle = colors.bg;
      ctx.lineWidth = 1.5;
      ctx.stroke();

      ctx.fillStyle = colors.success;
      ctx.font = 'bold 10px monospace';
      ctx.fillText(`True Root: ${fmtVal(state.trueRoot)}`, rx + 8, ry - 8);
    }
  }

  function renderBisectionConstruction(ctx, row, prevRow, toScreenX, toScreenY, colors, height, animT = 1.0) {
    const { a, b, c, fVal } = row;
    const sZero = toScreenY(0);

    // If animating and we have a previous row, interpolate the bracket boundaries
    let curA = a;
    let curB = b;
    if (animT < 1.0 && prevRow) {
      curA = prevRow.a + (a - prevRow.a) * animT;
      curB = prevRow.b + (b - prevRow.b) * animT;

      // Discarded half fading out
      if (a > prevRow.a) {
        // Left half was discarded [prevRow.a, a]
        const discA = toScreenX(prevRow.a);
        const discB = toScreenX(a);
        ctx.fillStyle = `rgba(239, 68, 68, ${0.18 * (1 - animT)})`;
        ctx.fillRect(Math.min(discA, discB), 0, Math.abs(discB - discA), height);
      } else if (b < prevRow.b) {
        // Right half was discarded [b, prevRow.b]
        const discA = toScreenX(b);
        const discB = toScreenX(prevRow.b);
        ctx.fillStyle = `rgba(239, 68, 68, ${0.18 * (1 - animT)})`;
        ctx.fillRect(Math.min(discA, discB), 0, Math.abs(discB - discA), height);
      }
    }

    const sa = toScreenX(curA);
    const sb = toScreenX(curB);
    const sc = toScreenX(c);

    // Shaded bracket region
    ctx.fillStyle = colors.bracketFill;
    ctx.fillRect(Math.min(sa, sb), 0, Math.abs(sb - sa), height);

    // Endpoint A
    ctx.strokeStyle = colors.danger;
    ctx.lineWidth = 1.5;
    ctx.beginPath();
    ctx.setLineDash([3, 3]);
    ctx.moveTo(sa, sZero);
    try {
      const fa = state.f(curA);
      ctx.lineTo(sa, toScreenY(fa));
      ctx.stroke();
      ctx.setLineDash([]);
      ctx.fillStyle = colors.danger;
      ctx.beginPath();
      ctx.arc(sa, toScreenY(fa), 5, 0, Math.PI * 2);
      ctx.fill();
    } catch (e) {
      ctx.setLineDash([]);
    }

    // Endpoint B
    ctx.strokeStyle = colors.success;
    ctx.lineWidth = 1.5;
    ctx.beginPath();
    ctx.setLineDash([3, 3]);
    ctx.moveTo(sb, sZero);
    try {
      const fb = state.f(curB);
      ctx.lineTo(sb, toScreenY(fb));
      ctx.stroke();
      ctx.setLineDash([]);
      ctx.fillStyle = colors.success;
      ctx.beginPath();
      ctx.arc(sb, toScreenY(fb), 5, 0, Math.PI * 2);
      ctx.fill();
    } catch (e) {
      ctx.setLineDash([]);
    }

    // Midpoint drop animation
    const fcY = toScreenY(fVal);
    // As animT goes from 0 to 1, midpoint drops from curve fcY down to sZero
    const curScY = animT < 1.0 ? fcY + (sZero - fcY) * animT : sZero;

    ctx.strokeStyle = colors.primary;
    ctx.lineWidth = 2;
    ctx.beginPath();
    ctx.setLineDash([4, 2]);
    ctx.moveTo(sc, sZero);
    ctx.lineTo(sc, fcY);
    ctx.stroke();
    ctx.setLineDash([]);

    // Curve point (c, f(c))
    ctx.fillStyle = colors.primary;
    ctx.beginPath();
    ctx.arc(sc, fcY, 5, 0, Math.PI * 2);
    ctx.fill();

    // Midpoint marker dropping to axis
    ctx.beginPath();
    ctx.arc(sc, curScY, 6, 0, Math.PI * 2);
    ctx.fill();

    // Ripple expansion ring when landing
    if (animT > 0.5 && animT < 1.0) {
      const rippleFrac = (animT - 0.5) / 0.5;
      ctx.strokeStyle = colors.primary;
      ctx.lineWidth = 1.5;
      ctx.beginPath();
      ctx.arc(sc, sZero, 6 + rippleFrac * 18, 0, Math.PI * 2);
      ctx.stroke();
    }

    // Label
    ctx.fillStyle = colors.textMain;
    ctx.font = 'bold 11px sans-serif';
    ctx.fillText(`c = ${fmtVal(c)}`, sc + 6, sZero - 8);
  }

  function renderSecantConstruction(ctx, row, toScreenX, toScreenY, colors, animT = 1.0) {
    const { p0, p1, p2, f0, f1 } = row;
    const s0x = toScreenX(p0);
    const s0y = toScreenY(f0);
    const s1x = toScreenX(p1);
    const s1y = toScreenY(f1);
    const s2x = toScreenX(p2);
    const s2y = toScreenY(0);

    // Points on curve
    ctx.fillStyle = colors.warning;
    ctx.beginPath();
    ctx.arc(s0x, s0y, 5, 0, Math.PI * 2);
    ctx.arc(s1x, s1y, 5, 0, Math.PI * 2);
    ctx.fill();

    // In animated mode: phase 1 (0..0.7) draws secant chord, phase 2 (0.7..1) drops line and pulses intercept
    const lineProgress = animT < 1.0 ? Math.min(animT / 0.7, 1.0) : 1.0;

    ctx.strokeStyle = colors.warning;
    ctx.lineWidth = 2;
    ctx.beginPath();

    const minX = Math.min(s0x, s1x, s2x) - 30;
    const slope = (s1y - s0y) / (s1x - s0x);
    const fullEnd = s2x + (s2x > s1x ? 25 : -25);
    const curEnd = minX + (fullEnd - minX) * lineProgress;

    ctx.moveTo(minX, s0y + slope * (minX - s0x));
    ctx.lineTo(curEnd, s0y + slope * (curEnd - s0x));
    ctx.stroke();

    // Intercept dot & vertical dashed line
    if (animT >= 0.6) {
      const dropProgress = animT < 1.0 ? (animT - 0.6) / 0.4 : 1.0;
      ctx.strokeStyle = colors.warning;
      ctx.setLineDash([3, 3]);
      ctx.beginPath();
      ctx.moveTo(s2x, s2y);
      try {
        const fp2Y = toScreenY(state.f(p2));
        ctx.lineTo(s2x, s2y + (fp2Y - s2y) * dropProgress);
      } catch (e) {}
      ctx.stroke();
      ctx.setLineDash([]);

      ctx.fillStyle = colors.primary;
      ctx.beginPath();
      ctx.arc(s2x, s2y, 6, 0, Math.PI * 2);
      ctx.fill();

      // Ripple ring on intercept landing
      if (animT > 0.7 && animT < 1.0) {
        const rFrac = (animT - 0.7) / 0.3;
        ctx.strokeStyle = colors.primary;
        ctx.lineWidth = 1.5;
        ctx.beginPath();
        ctx.arc(s2x, s2y, 6 + rFrac * 18, 0, Math.PI * 2);
        ctx.stroke();
      }

      ctx.fillStyle = colors.textMain;
      ctx.font = 'bold 11px sans-serif';
      ctx.fillText(`x_(n+1) = ${fmtVal(p2)}`, s2x + 6, s2y - 8);
    }
  }

  function renderNewtonConstruction(ctx, row, toScreenX, toScreenY, colors, animT = 1.0) {
    const { xn, fVal, dfVal } = row;
    const sx = toScreenX(xn);
    const sy = toScreenY(fVal);
    const sZero = toScreenY(0);

    // Phase 1 (0..0.35): vertical line rises up from x-axis to curve point
    const vertProgress = animT < 1.0 ? Math.min(animT / 0.35, 1.0) : 1.0;

    ctx.strokeStyle = colors.danger;
    ctx.lineWidth = 1.5;
    ctx.beginPath();
    ctx.setLineDash([3, 3]);
    ctx.moveTo(sx, sZero);
    ctx.lineTo(sx, sZero + (sy - sZero) * vertProgress);
    ctx.stroke();
    ctx.setLineDash([]);

    if (animT >= 0.25) {
      ctx.fillStyle = colors.danger;
      ctx.beginPath();
      ctx.arc(sx, sy, 5.5, 0, Math.PI * 2);
      ctx.fill();
    }

    if (Math.abs(dfVal) > 1e-14 && animT >= 0.35) {
      const xNext = xn - fVal / dfVal;
      const sNextX = toScreenX(xNext);

      // Phase 2 (0.35..0.85): tangent line shoots out from (sx, sy) down to (sNextX, sZero)
      const tangProgress = animT < 1.0 ? Math.min((animT - 0.35) / 0.5, 1.0) : 1.0;
      const curEndX = sx + (sNextX - sx) * tangProgress;
      const curEndY = sy + (sZero - sy) * tangProgress;

      ctx.strokeStyle = colors.primary;
      ctx.lineWidth = 2;
      ctx.beginPath();
      const ext = (sNextX - sx) * 0.15;
      ctx.moveTo(sx - ext * tangProgress, sy - ext * tangProgress * ((sZero - sy) / (sNextX - sx)));
      ctx.lineTo(curEndX, curEndY);
      ctx.stroke();

      // Phase 3 (0.75..1.0): intercept marker & ripple ring
      if (animT >= 0.75) {
        ctx.fillStyle = colors.primary;
        ctx.beginPath();
        ctx.arc(sNextX, sZero, 6, 0, Math.PI * 2);
        ctx.fill();

        if (animT > 0.8 && animT < 1.0) {
          const rFrac = (animT - 0.8) / 0.2;
          ctx.strokeStyle = colors.primary;
          ctx.lineWidth = 1.5;
          ctx.beginPath();
          ctx.arc(sNextX, sZero, 6 + rFrac * 18, 0, Math.PI * 2);
          ctx.stroke();
        }

        ctx.fillStyle = colors.textMain;
        ctx.font = 'bold 11px sans-serif';
        ctx.fillText(`x_(n+1) = ${fmtVal(xNext)}`, sNextX + 6, sZero - 8);
      }
    }
  }

  /**
   * Panel 2: Error Trajectory Semi-Log Decay Plot
   */
  function renderErrorCanvas() {
    const canvas = el.errorCanvas;
    if (!canvas) return;
    const { width, height, ctx } = resizeCanvas(canvas);
    const colors = getThemeColors();

    ctx.clearRect(0, 0, width, height);

    const result = state.currentResult;
    if (!result || result.rows.length === 0) {
      ctx.fillStyle = colors.text;
      ctx.font = '12px sans-serif';
      ctx.textAlign = 'center';
      ctx.fillText('Execute solver to visualize error convergence trajectory.', width / 2, height / 2);
      return;
    }

    const padding = { top: 25, right: 30, bottom: 35, left: 55 };
    const plotW = width - padding.left - padding.right;
    const plotH = height - padding.top - padding.bottom;

    // Filter valid error points (relative error in %)
    const errorPoints = result.rows
      .map((r, idx) => ({
        iter: r.iteration,
        relEaPct: r.relEa !== null && isFinite(r.relEa) ? r.relEa * 100 : null
      }))
      .filter(p => p.relEaPct !== null && p.relEaPct > 0);

    if (errorPoints.length === 0) {
      ctx.fillStyle = colors.text;
      ctx.font = '12px sans-serif';
      ctx.textAlign = 'center';
      ctx.fillText('Single iteration completed. No error decay trajectory.', width / 2, height / 2);
      return;
    }

    const tolPct = state.tol * 100;
    const allErrors = errorPoints.map(p => p.relEaPct).concat([tolPct]);
    const minErr = Math.min(...allErrors);
    const maxErr = Math.max(...allErrors);

    // Log10 range
    const logMin = Math.floor(Math.log10(Math.max(minErr, 1e-14)));
    const logMax = Math.ceil(Math.log10(Math.max(maxErr, 1e-2)));
    const logRange = Math.max(logMax - logMin, 1);

    const maxIter = Math.max(result.rows[result.rows.length - 1].iteration, 2);

    const toScreenX = (iter) => padding.left + ((iter - 1) / (maxIter - 1)) * plotW;
    const toScreenY = (val) => {
      const logV = Math.log10(Math.max(val, 1e-16));
      return padding.top + ((logMax - logV) / logRange) * plotH;
    };

    // 1. Draw Grid Lines & Log Ticks
    ctx.strokeStyle = colors.grid;
    ctx.lineWidth = 1;

    for (let p = logMin; p <= logMax; p++) {
      const sy = padding.top + ((logMax - p) / logRange) * plotH;
      ctx.beginPath();
      ctx.moveTo(padding.left, sy);
      ctx.lineTo(width - padding.right, sy);
      ctx.stroke();

      ctx.fillStyle = colors.text;
      ctx.font = '10px monospace';
      ctx.textAlign = 'right';
      ctx.fillText(`10^${p}%`, padding.left - 6, sy + 3);
    }

    // Iteration X ticks
    for (let it = 1; it <= maxIter; it += Math.max(1, Math.floor(maxIter / 8))) {
      const sx = toScreenX(it);
      ctx.beginPath();
      ctx.moveTo(sx, padding.top);
      ctx.lineTo(sx, height - padding.bottom);
      ctx.stroke();

      ctx.fillStyle = colors.text;
      ctx.font = '10px monospace';
      ctx.textAlign = 'center';
      ctx.fillText(`k=${it}`, sx, height - padding.bottom + 16);
    }

    // 2. Plot Tolerance Threshold Dashed Line
    const tolY = toScreenY(tolPct);
    ctx.strokeStyle = colors.primary;
    ctx.lineWidth = 1.8;
    ctx.setLineDash([5, 4]);
    ctx.beginPath();
    ctx.moveTo(padding.left, tolY);
    ctx.lineTo(width - padding.right, tolY);
    ctx.stroke();
    ctx.setLineDash([]);

    ctx.fillStyle = colors.primary;
    ctx.font = 'bold 10px monospace';
    ctx.textAlign = 'left';
    ctx.fillText(`Tol (${fmtVal(tolPct, 4)}%)`, padding.left + 8, tolY - 5);

    // 3. Plot Relative Error Decay Line
    ctx.strokeStyle = colors.danger;
    ctx.lineWidth = 2.2;
    ctx.beginPath();

    errorPoints.forEach((pt, i) => {
      const sx = toScreenX(pt.iter);
      const sy = toScreenY(pt.relEaPct);
      if (i === 0) ctx.moveTo(sx, sy);
      else ctx.lineTo(sx, sy);
    });
    ctx.stroke();

    // 4. Highlight dots
    const activeStep = state.playback.currentStep;

    errorPoints.forEach((pt) => {
      const sx = toScreenX(pt.iter);
      const sy = toScreenY(pt.relEaPct);
      const isActive = pt.iter === activeStep;

      ctx.fillStyle = isActive ? colors.primary : colors.danger;
      ctx.beginPath();
      ctx.arc(sx, sy, isActive ? 6 : 3.5, 0, Math.PI * 2);
      ctx.fill();

      if (isActive) {
        ctx.strokeStyle = colors.bg;
        ctx.lineWidth = 2;
        ctx.stroke();
      }
    });
  }

  /**
   * Shootout Comparison Convergence Canvas
   */
  function renderShootoutCanvas() {
    const canvas = el.shootoutCanvas;
    if (!canvas) return;
    const { width, height, ctx } = resizeCanvas(canvas);
    const colors = getThemeColors();

    ctx.clearRect(0, 0, width, height);

    const sResults = state.shootoutResults;
    if (!sResults) return;

    const padding = { top: 25, right: 140, bottom: 35, left: 60 };
    const plotW = width - padding.left - padding.right;
    const plotH = height - padding.top - padding.bottom;

    const series = [
      { name: 'Bisection', res: sResults.bisection, color: colors.primary },
      { name: 'Secant', res: sResults.secant, color: colors.warning },
      { name: 'Newton', res: sResults.newton, color: colors.success }
    ];

    let maxIter = 2;
    let minErr = 100;
    let maxErr = 1e-4;

    series.forEach(s => {
      if (!s.res || !s.res.rows) return;
      s.res.rows.forEach(r => {
        if (r.iteration > maxIter) maxIter = r.iteration;
        if (r.relEa !== null && isFinite(r.relEa) && r.relEa > 0) {
          const val = r.relEa * 100;
          if (val < minErr) minErr = val;
          if (val > maxErr) maxErr = val;
        }
      });
    });

    const tolPct = state.tol * 100;
    minErr = Math.min(minErr, tolPct);

    const logMin = Math.floor(Math.log10(Math.max(minErr, 1e-15)));
    const logMax = Math.ceil(Math.log10(Math.max(maxErr, 1e1)));
    const logRange = Math.max(logMax - logMin, 1);

    const toScreenX = (iter) => padding.left + ((iter - 1) / (maxIter - 1)) * plotW;
    const toScreenY = (val) => {
      const logV = Math.log10(Math.max(val, 1e-16));
      return padding.top + ((logMax - logV) / logRange) * plotH;
    };

    // Draw grid
    ctx.strokeStyle = colors.grid;
    for (let p = logMin; p <= logMax; p += 2) {
      const sy = padding.top + ((logMax - p) / logRange) * plotH;
      ctx.beginPath();
      ctx.moveTo(padding.left, sy);
      ctx.lineTo(width - padding.right, sy);
      ctx.stroke();

      ctx.fillStyle = colors.text;
      ctx.font = '10px monospace';
      ctx.textAlign = 'right';
      ctx.fillText(`10^${p}%`, padding.left - 6, sy + 3);
    }

    // Tolerance Line
    const tolY = toScreenY(tolPct);
    ctx.strokeStyle = colors.danger;
    ctx.setLineDash([4, 4]);
    ctx.beginPath();
    ctx.moveTo(padding.left, tolY);
    ctx.lineTo(width - padding.right, tolY);
    ctx.stroke();
    ctx.setLineDash([]);

    // Draw each series
    series.forEach((s) => {
      if (!s.res || !s.res.rows) return;
      const pts = s.res.rows
        .map(r => ({ iter: r.iteration, pct: r.relEa !== null && isFinite(r.relEa) ? r.relEa * 100 : null }))
        .filter(p => p.pct !== null && p.pct > 0);

      if (pts.length === 0) return;

      ctx.strokeStyle = s.color;
      ctx.lineWidth = 2.4;
      ctx.beginPath();
      pts.forEach((p, i) => {
        const sx = toScreenX(p.iter);
        const sy = toScreenY(p.pct);
        if (i === 0) ctx.moveTo(sx, sy);
        else ctx.lineTo(sx, sy);
      });
      ctx.stroke();

      pts.forEach(p => {
        const sx = toScreenX(p.iter);
        const sy = toScreenY(p.pct);
        ctx.fillStyle = s.color;
        ctx.beginPath();
        ctx.arc(sx, sy, 3.5, 0, Math.PI * 2);
        ctx.fill();
      });
    });

    // Legend
    let legendY = padding.top + 10;
    series.forEach(s => {
      ctx.fillStyle = s.color;
      ctx.fillRect(width - padding.right + 15, legendY, 14, 14);
      ctx.fillStyle = colors.textMain;
      ctx.font = 'bold 11px sans-serif';
      ctx.textAlign = 'left';
      ctx.fillText(s.name, width - padding.right + 35, legendY + 11);
      legendY += 24;
    });

    // Tol legend
    ctx.fillStyle = colors.danger;
    ctx.fillRect(width - padding.right + 15, legendY, 14, 3);
    ctx.fillStyle = colors.text;
    ctx.font = '10px sans-serif';
    ctx.fillText('Tolerance', width - padding.right + 35, legendY + 6);
  }

  function renderCanvases() {
    renderGeomCanvas();
    renderErrorCanvas();
    renderShootoutCanvas();
  }

  /* ==========================================================================
     7. CANVAS INTERACTIVITY (PAN, ZOOM, TOOLTIP)
     ========================================================================== */

  /* ==========================================================================
     7. CANVAS INTERACTIVITY (PAN, SELECT ZOOM, TOOLTIPS)
     ========================================================================== */

  function animateViewToBounds(targetXMin, targetXMax, targetYMin, targetYMax, duration = null) {
    if (state.view.zoomAnimId) {
      cancelAnimationFrame(state.view.zoomAnimId);
      state.view.zoomAnimId = null;
    }

    const animDuration = duration !== null ? duration : Math.max(40, Math.round(state.playback.speed * 0.85));
    const startXMin = state.view.xMin;
    const startXMax = state.view.xMax;
    const startYMin = state.view.yMin;
    const startYMax = state.view.yMax;
    const startTime = performance.now();

    function step(now) {
      const elapsed = now - startTime;
      const progress = Math.min(elapsed / animDuration, 1.0);
      // ease-out cubic
      const ease = 1 - Math.pow(1 - progress, 3);

      state.view.xMin = startXMin + (targetXMin - startXMin) * ease;
      state.view.xMax = startXMax + (targetXMax - startXMax) * ease;
      state.view.yMin = startYMin + (targetYMin - startYMin) * ease;
      state.view.yMax = startYMax + (targetYMax - startYMax) * ease;

      renderGeomCanvas();

      if (progress < 1.0) {
        state.view.zoomAnimId = requestAnimationFrame(step);
      } else {
        state.view.zoomAnimId = null;
      }
    }

    state.view.zoomAnimId = requestAnimationFrame(step);
  }

  /**
   * Computes focused viewport bounds (xMin, xMax, yMin, yMax) for a given iteration step.
   * Enables smooth auto-zooming into active brackets, secant chords, or tangent lines.
   */
  function getStepViewBounds(stepNumber) {
    const result = state.currentResult;
    if (!result || !result.rows || result.rows.length === 0) {
      return {
        xMin: state.view.origXMin,
        xMax: state.view.origXMax,
        yMin: state.view.origYMin,
        yMax: state.view.origYMax
      };
    }

    if (stepNumber <= 1) {
      return {
        xMin: state.view.origXMin,
        xMax: state.view.origXMax,
        yMin: state.view.origYMin,
        yMax: state.view.origYMax
      };
    }

    const idx = Math.max(0, Math.min(stepNumber - 1, result.rows.length - 1));
    const row = result.rows[idx];

    let targetXMin, targetXMax;
    let keyYValues = [0];

    if (state.method === 'bisection') {
      const a = row.a;
      const b = row.b;
      const left = Math.min(a, b);
      const right = Math.max(a, b);
      const span = Math.max(right - left, 2e-5);
      const padding = Math.max(span * 0.45, 1e-5);

      targetXMin = left - padding;
      targetXMax = right + padding;

      keyYValues.push(row.fVal);
      try { keyYValues.push(state.f(a), state.f(b)); } catch (e) {}
    } else if (state.method === 'secant') {
      const p0 = row.p0;
      const p1 = row.p1;
      const p2 = row.p2;
      const minX = Math.min(p0, p1, p2);
      const maxX = Math.max(p0, p1, p2);
      const span = Math.max(maxX - minX, 2e-5);
      const padding = Math.max(span * 0.4, 1e-5);

      targetXMin = minX - padding;
      targetXMax = maxX + padding;

      keyYValues.push(row.f0, row.f1, row.fVal);
    } else if (state.method === 'newton') {
      const xn = row.xn;
      const df = row.dfVal;
      const xNext = Math.abs(df) > 1e-14 ? xn - row.fVal / df : xn;
      const minX = Math.min(xn, xNext);
      const maxX = Math.max(xn, xNext);
      const span = Math.max(maxX - minX, 2e-5);
      const padding = Math.max(span * 0.5, 1e-5);

      targetXMin = minX - padding;
      targetXMax = maxX + padding;

      keyYValues.push(row.fVal);
      try { keyYValues.push(state.f(xNext)); } catch (e) {}
    } else {
      return {
        xMin: state.view.origXMin,
        xMax: state.view.origXMax,
        yMin: state.view.origYMin,
        yMax: state.view.origYMax
      };
    }

    // Sample function values across the target x-interval to ensure curve stays in view
    if (state.f) {
      const numSamples = 16;
      for (let i = 0; i <= numSamples; i++) {
        const sx = targetXMin + (i / numSamples) * (targetXMax - targetXMin);
        try {
          const sy = state.f(sx);
          if (isFinite(sy) && !isNaN(sy) && Math.abs(sy) < 1e5) {
            keyYValues.push(sy);
          }
        } catch (e) {}
      }
    }

    const validY = keyYValues.filter(v => isFinite(v) && !isNaN(v));
    let minY = validY.length > 0 ? Math.min(...validY) : -1;
    let maxY = validY.length > 0 ? Math.max(...validY) : 1;

    let ySpan = Math.max(maxY - minY, 0.04);
    if (ySpan > 1000) ySpan = 1000;
    const paddingY = Math.max(ySpan * 0.35, 0.02);

    let targetYMin = minY - paddingY;
    let targetYMax = maxY + paddingY;

    // Ensure the x-axis (y = 0) is comfortably visible
    if (targetYMin > -paddingY * 0.35) targetYMin = -paddingY * 0.35;
    if (targetYMax < paddingY * 0.35) targetYMax = paddingY * 0.35;

    return {
      xMin: targetXMin,
      xMax: targetXMax,
      yMin: targetYMin,
      yMax: targetYMax
    };
  }

  function initCanvasInteractions() {
    const canvas = el.geomCanvas;
    if (!canvas) return;

    // Auto-fit function view bounds based on root and brackets
    function autoFitView(a, b, root) {
      const r = isFinite(root) && root !== null ? root : (a + b) / 2;
      const span = Math.max(Math.abs(b - a), 1.5);
      state.view.xMin = r - span * 1.4;
      state.view.xMax = r + span * 1.4;

      // Sample y values to compute suitable y span
      let minVal = -1;
      let maxVal = 1;
      if (state.f) {
        for (let i = 0; i <= 30; i++) {
          const tx = state.view.xMin + (i / 30) * (state.view.xMax - state.view.xMin);
          try {
            const v = state.f(tx);
            if (isFinite(v) && !isNaN(v)) {
              if (v < minVal) minVal = v;
              if (v > maxVal) maxVal = v;
            }
          } catch (e) {}
        }
      }
      const ySpan = Math.max(maxVal - minVal, 2);
      state.view.yMin = minVal - ySpan * 0.25;
      state.view.yMax = maxVal + ySpan * 0.25;

      // Record original reset bounds
      state.view.origXMin = state.view.xMin;
      state.view.origXMax = state.view.xMax;
      state.view.origYMin = state.view.yMin;
      state.view.origYMax = state.view.yMax;

      renderGeomCanvas();
    }

    // Set Active Tool Mode: Pan vs Select Zoom
    function setTool(toolName) {
      state.view.tool = toolName;
      if (el.toolPan) {
        el.toolPan.classList.toggle('active', toolName === 'pan');
        el.toolPan.setAttribute('aria-pressed', toolName === 'pan');
      }
      if (el.toolSelectZoom) {
        el.toolSelectZoom.classList.toggle('active', toolName === 'select-zoom');
        el.toolSelectZoom.setAttribute('aria-pressed', toolName === 'select-zoom');
      }
      if (el.canvasHint) {
        el.canvasHint.innerHTML = toolName === 'select-zoom' ?
          'Drag a box to zoom into area &bull; Double-click to reset' :
          'Drag to pan &bull; Shift+drag to box zoom &bull; Scroll to zoom';
      }
      canvas.style.cursor = toolName === 'select-zoom' ? 'crosshair' : 'grab';
    }

    if (el.toolPan) {
      el.toolPan.addEventListener('click', () => setTool('pan'));
    }
    if (el.toolSelectZoom) {
      el.toolSelectZoom.addEventListener('click', () => setTool('select-zoom'));
    }

    // Zoom In / Zoom Out toolbar buttons
    function zoomByFactor(factor) {
      const midX = (state.view.xMin + state.view.xMax) / 2;
      const midY = (state.view.yMin + state.view.yMax) / 2;
      const halfX = ((state.view.xMax - state.view.xMin) * factor) / 2;
      const halfY = ((state.view.yMax - state.view.yMin) * factor) / 2;
      animateViewToBounds(midX - halfX, midX + halfX, midY - halfY, midY + halfY, 250);
    }

    if (el.btnZoomIn) {
      el.btnZoomIn.addEventListener('click', () => zoomByFactor(0.72));
    }
    if (el.btnZoomOut) {
      el.btnZoomOut.addEventListener('click', () => zoomByFactor(1.38));
    }

    // Reset Viewport button
    if (el.btnResetView) {
      el.btnResetView.addEventListener('click', () => {
        animateViewToBounds(state.view.origXMin, state.view.origXMax, state.view.origYMin, state.view.origYMax, 300);
      });
    }

    // Double-click on canvas resets to initial framing
    canvas.addEventListener('dblclick', () => {
      animateViewToBounds(state.view.origXMin, state.view.origXMax, state.view.origYMin, state.view.origYMax, 300);
    });

    function updateSelectionBox() {
      if (!el.canvasSelectionBox) return;
      const sx1 = Math.min(state.selection.startX, state.selection.currentX);
      const sx2 = Math.max(state.selection.startX, state.selection.currentX);
      const sy1 = Math.min(state.selection.startY, state.selection.currentY);
      const sy2 = Math.max(state.selection.startY, state.selection.currentY);

      el.canvasSelectionBox.style.left = `${sx1}px`;
      el.canvasSelectionBox.style.top = `${sy1}px`;
      el.canvasSelectionBox.style.width = `${sx2 - sx1}px`;
      el.canvasSelectionBox.style.height = `${sy2 - sy1}px`;

      const rect = canvas.getBoundingClientRect();
      const xSpan = state.view.xMax - state.view.xMin;
      const ySpan = state.view.yMax - state.view.yMin;
      const dx = Math.abs((sx2 - sx1) / rect.width * xSpan);
      const dy = Math.abs((sy2 - sy1) / rect.height * ySpan);

      if (el.canvasSelectionLabel) {
        el.canvasSelectionLabel.textContent = `Δx: ${cleanNum(dx, 1e-4).toFixed(3)}, Δy: ${cleanNum(dy, 1e-4).toFixed(3)}`;
      }
    }

    // Mouse Down: Start Pan or Select Zoom
    canvas.addEventListener('mousedown', (e) => {
      const rect = canvas.getBoundingClientRect();
      const mouseX = e.clientX - rect.left;
      const mouseY = e.clientY - rect.top;

      const isSelectZoom = state.view.tool === 'select-zoom' || e.shiftKey;

      if (isSelectZoom) {
        state.selection.active = true;
        state.selection.startX = mouseX;
        state.selection.startY = mouseY;
        state.selection.currentX = mouseX;
        state.selection.currentY = mouseY;
        updateSelectionBox();
        if (el.canvasSelectionBox) el.canvasSelectionBox.style.display = 'block';
      } else {
        state.view.isDragging = true;
        state.view.dragStartX = e.clientX;
        state.view.dragStartY = e.clientY;
        canvas.style.cursor = 'grabbing';
      }
    });

    window.addEventListener('mousemove', (e) => {
      // 1. Handling Box Selection Drag
      if (state.selection.active) {
        const rect = canvas.getBoundingClientRect();
        state.selection.currentX = Math.max(0, Math.min(rect.width, e.clientX - rect.left));
        state.selection.currentY = Math.max(0, Math.min(rect.height, e.clientY - rect.top));
        updateSelectionBox();
        return;
      }

      // 2. Handling Pan Drag
      if (state.view.isDragging) {
        const dx = e.clientX - state.view.dragStartX;
        const dy = e.clientY - state.view.dragStartY;
        state.view.dragStartX = e.clientX;
        state.view.dragStartY = e.clientY;

        const rect = canvas.getBoundingClientRect();
        const xRange = state.view.xMax - state.view.xMin;
        const yRange = state.view.yMax - state.view.yMin;

        const xShift = (dx / rect.width) * xRange;
        const yShift = (dy / rect.height) * yRange;

        state.view.xMin -= xShift;
        state.view.xMax -= xShift;
        state.view.yMin += yShift;
        state.view.yMax += yShift;

        renderGeomCanvas();
        return;
      }

      // 3. Hover coordinate tooltip
      handleCanvasHover(e);
    });

    window.addEventListener('mouseup', () => {
      // Finish Select Zoom Drag
      if (state.selection.active) {
        state.selection.active = false;
        if (el.canvasSelectionBox) el.canvasSelectionBox.style.display = 'none';

        const sx1 = Math.min(state.selection.startX, state.selection.currentX);
        const sx2 = Math.max(state.selection.startX, state.selection.currentX);
        const sy1 = Math.min(state.selection.startY, state.selection.currentY);
        const sy2 = Math.max(state.selection.startY, state.selection.currentY);

        const w = sx2 - sx1;
        const h = sy2 - sy1;

        if (w > 8 && h > 8) {
          const rect = canvas.getBoundingClientRect();
          const xSpan = state.view.xMax - state.view.xMin;
          const ySpan = state.view.yMax - state.view.yMin;

          const newXMin = state.view.xMin + (sx1 / rect.width) * xSpan;
          const newXMax = state.view.xMin + (sx2 / rect.width) * xSpan;
          // Invert Y because canvas Y=0 is at top
          const newYMax = state.view.yMax - (sy1 / rect.height) * ySpan;
          const newYMin = state.view.yMax - (sy2 / rect.height) * ySpan;

          animateViewToBounds(newXMin, newXMax, newYMin, newYMax, 300);
        }
      }

      if (state.view.isDragging) {
        state.view.isDragging = false;
        canvas.style.cursor = state.view.tool === 'select-zoom' ? 'crosshair' : 'grab';
      }
    });

    // Mouse Wheel to Zoom centered at cursor
    canvas.addEventListener('wheel', (e) => {
      e.preventDefault();
      const rect = canvas.getBoundingClientRect();
      const mouseX = e.clientX - rect.left;
      const mouseY = e.clientY - rect.top;

      const normX = mouseX / rect.width;
      const normY = (rect.height - mouseY) / rect.height;

      const factor = e.deltaY < 0 ? 0.85 : 1.18;

      const xRange = (state.view.xMax - state.view.xMin) * factor;
      const yRange = (state.view.yMax - state.view.yMin) * factor;

      const cursorMathX = state.view.xMin + normX * (state.view.xMax - state.view.xMin);
      const cursorMathY = state.view.yMin + normY * (state.view.yMax - state.view.yMin);

      state.view.xMin = cursorMathX - normX * xRange;
      state.view.xMax = cursorMathX + (1 - normX) * xRange;
      state.view.yMin = cursorMathY - normY * yRange;
      state.view.yMax = cursorMathY + (1 - normY) * yRange;

      renderGeomCanvas();
    }, { passive: false });

    function handleCanvasHover(e) {
      const rect = canvas.getBoundingClientRect();
      if (
        e.clientX < rect.left || e.clientX > rect.right ||
        e.clientY < rect.top || e.clientY > rect.bottom
      ) {
        if (el.canvasTooltip) el.canvasTooltip.style.display = 'none';
        return;
      }

      const mx = e.clientX - rect.left;
      const my = e.clientY - rect.top;
      const mathX = state.view.xMin + (mx / rect.width) * (state.view.xMax - state.view.xMin);

      let mathY = null;
      if (state.f) {
        try {
          mathY = state.f(mathX);
        } catch (err) {}
      }

      if (mathY !== null && isFinite(mathY)) {
        if (el.canvasTooltip) {
          el.canvasTooltip.style.display = 'block';
          el.canvasTooltip.style.left = `${mx + 12}px`;
          el.canvasTooltip.style.top = `${my - 28}px`;
          el.canvasTooltip.innerHTML = `x: ${fmtVal(mathX, 4)}<br>f(x): ${fmtVal(mathY, 4)}`;
        }
      } else {
        if (el.canvasTooltip) el.canvasTooltip.style.display = 'none';
      }
    }

    state.autoFitView = autoFitView;
  }

  /* ==========================================================================
     8. PLAYBACK CONTROLLER WITH ANIMATED STEP TRANSITIONS
     ========================================================================== */

  function initPlayback() {
    function getAnimationDuration(speed) {
      // Scale animation duration proportionally to playback speed:
      // ~85% of step interval provides smooth continuous motion and responsive pacing
      return Math.max(40, Math.round(speed * 0.85));
    }

    function setStep(step, animate = true) {
      if (!state.currentResult || state.currentResult.rows.length === 0) return;
      const total = state.currentResult.rows.length;
      const target = Math.max(1, Math.min(step, total));
      const fromStep = state.playback.currentStep;

      if (state.stepAnimation.reqId) {
        cancelAnimationFrame(state.stepAnimation.reqId);
        state.stepAnimation.reqId = null;
      }
      if (state.view.zoomAnimId) {
        cancelAnimationFrame(state.view.zoomAnimId);
        state.view.zoomAnimId = null;
      }

      state.playback.currentStep = target;

      // Update both main and fullscreen scrubbers
      if (el.stepSlider) el.stepSlider.value = target;
      if (el.fsStepSlider) el.fsStepSlider.value = target;

      const badgeText = `Step ${target} / ${total}`;
      if (el.stepBadge) el.stepBadge.textContent = badgeText;
      if (el.fsStepBadge) el.fsStepBadge.textContent = badgeText;
      if (el.geomStepLabel) el.geomStepLabel.textContent = `Iteration k = ${target}`;

      updateStepDetail(target);
      highlightTableRow(target);
      renderErrorCanvas();

      const shouldAnimate = state.stepAnimation.enabled && animate && fromStep !== target;

      if (shouldAnimate) {
        state.stepAnimation.active = true;
        state.stepAnimation.fromStep = fromStep;
        state.stepAnimation.toStep = target;
        state.stepAnimation.duration = getAnimationDuration(state.playback.speed);
        state.stepAnimation.startTime = performance.now();

        // Auto Zoom: smoothly transition viewport bounds alongside the mathematical construction
        const shouldAutoZoom = state.view.autoZoom;
        const startXMin = state.view.xMin;
        const startXMax = state.view.xMax;
        const startYMin = state.view.yMin;
        const startYMax = state.view.yMax;
        const targetBounds = shouldAutoZoom ? getStepViewBounds(target) : null;

        function frame(now) {
          const elapsed = now - state.stepAnimation.startTime;
          const progress = Math.min(elapsed / state.stepAnimation.duration, 1.0);
          // Ease-out cubic for smooth natural deceleration
          const ease = 1 - Math.pow(1 - progress, 3);
          state.stepAnimation.t = ease;

          if (shouldAutoZoom && targetBounds) {
            state.view.xMin = startXMin + (targetBounds.xMin - startXMin) * ease;
            state.view.xMax = startXMax + (targetBounds.xMax - startXMax) * ease;
            state.view.yMin = startYMin + (targetBounds.yMin - startYMin) * ease;
            state.view.yMax = startYMax + (targetBounds.yMax - startYMax) * ease;
          }

          renderGeomCanvas();

          if (progress < 1.0) {
            state.stepAnimation.reqId = requestAnimationFrame(frame);
          } else {
            state.stepAnimation.active = false;
            state.stepAnimation.t = 1.0;
            state.stepAnimation.reqId = null;
            if (shouldAutoZoom && targetBounds) {
              state.view.xMin = targetBounds.xMin;
              state.view.xMax = targetBounds.xMax;
              state.view.yMin = targetBounds.yMin;
              state.view.yMax = targetBounds.yMax;
            }
            renderGeomCanvas();
          }
        }

        state.stepAnimation.reqId = requestAnimationFrame(frame);
      } else {
        state.stepAnimation.active = false;
        state.stepAnimation.t = 1.0;
        if (state.view.autoZoom) {
          const targetBounds = getStepViewBounds(target);
          state.view.xMin = targetBounds.xMin;
          state.view.xMax = targetBounds.xMax;
          state.view.yMin = targetBounds.yMin;
          state.view.yMax = targetBounds.yMax;
        }
        renderGeomCanvas();
      }
    }

    // Synchronize Animation Toggles
    function syncAnimToggle(enabled) {
      state.stepAnimation.enabled = enabled;
      if (el.toggleAnimSteps) el.toggleAnimSteps.checked = enabled;
      if (el.fsToggleAnimSteps) el.fsToggleAnimSteps.checked = enabled;
    }

    if (el.toggleAnimSteps) {
      el.toggleAnimSteps.addEventListener('change', (e) => syncAnimToggle(e.target.checked));
    }
    if (el.fsToggleAnimSteps) {
      el.fsToggleAnimSteps.addEventListener('change', (e) => syncAnimToggle(e.target.checked));
    }

    // Synchronize Auto Zoom Toggles
    function syncAutoZoomToggle(enabled) {
      state.view.autoZoom = enabled;
      if (el.toggleAutoZoom) el.toggleAutoZoom.checked = enabled;
      if (el.fsToggleAutoZoom) el.fsToggleAutoZoom.checked = enabled;

      if (enabled && state.currentResult && state.currentResult.rows.length > 0) {
        const bounds = getStepViewBounds(state.playback.currentStep);
        const dur = getAnimationDuration(state.playback.speed);
        animateViewToBounds(bounds.xMin, bounds.xMax, bounds.yMin, bounds.yMax, dur);
      }
    }

    if (el.toggleAutoZoom) {
      el.toggleAutoZoom.addEventListener('change', (e) => syncAutoZoomToggle(e.target.checked));
    }
    if (el.fsToggleAutoZoom) {
      el.fsToggleAutoZoom.addEventListener('change', (e) => syncAutoZoomToggle(e.target.checked));
    }

    // Synchronize Speed Controls (Main and Fullscreen HUD)
    function syncSpeed(newSpeed) {
      const val = parseInt(newSpeed, 10);
      state.playback.speed = val;
      if (el.playbackSpeed) el.playbackSpeed.value = newSpeed;
      if (el.fsPlaybackSpeed) el.fsPlaybackSpeed.value = newSpeed;

      // Dynamically adjust ongoing step animation and auto zoom duration in real time
      if (state.stepAnimation.active && state.stepAnimation.duration > 0) {
        const now = performance.now();
        const oldDuration = state.stepAnimation.duration;
        const newDuration = getAnimationDuration(val);
        const currentProgress = Math.min((now - state.stepAnimation.startTime) / oldDuration, 1.0);
        state.stepAnimation.duration = newDuration;
        state.stepAnimation.startTime = now - (currentProgress * newDuration);
      }

      // If playing, restart the timer with updated speed
      if (state.playback.isPlaying) {
        pause();
        play();
      }
    }

    if (el.playbackSpeed) {
      el.playbackSpeed.addEventListener('change', (e) => syncSpeed(e.target.value));
    }
    if (el.fsPlaybackSpeed) {
      el.fsPlaybackSpeed.addEventListener('change', (e) => syncSpeed(e.target.value));
    }

    function updateStepDetail(step) {
      const res = state.currentResult;
      if (!res || !res.rows[step - 1]) return;
      const row = res.rows[step - 1];

      if (el.stepNumberTag) el.stepNumberTag.textContent = `Iteration ${row.iteration}`;
      if (el.stepActionDesc) el.stepActionDesc.textContent = row.desc;

      let mathHtml = '';
      if (state.method === 'bisection') {
        const fc = fmtVal(row.fVal);
        const signComp = state.f(row.a) * row.fVal < 0 ? '< 0 (Next interval: [a, c])' : '> 0 (Next interval: [c, b])';
        mathHtml = `
          <div><strong>Interval Bracket:</strong> [${fmtVal(row.a)}, ${fmtVal(row.b)}] &bull; <strong>Width:</strong> ${fmtVal(row.b - row.a)}</div>
          <div><strong>Midpoint:</strong> c = (${fmtVal(row.a)} + ${fmtVal(row.b)}) / 2 = <strong>${fmtVal(row.c)}</strong></div>
          <div><strong>f(c):</strong> ${fc} &bull; <strong>Bolzano Check:</strong> f(a)&middot;f(c) ${signComp}</div>
          <div><strong>Absolute Error E<sub>a</sub>:</strong> ${fmtError(row.ea)} &bull; <strong>Relative Error &epsilon;<sub>a</sub>:</strong> ${fmtRelError(row.relEa)}</div>
        `;
      } else if (state.method === 'secant') {
        mathHtml = `
          <div><strong>Points:</strong> x<sub>n-1</sub> = ${fmtVal(row.p0)}, x<sub>n</sub> = ${fmtVal(row.p1)}</div>
          <div><strong>Function Values:</strong> f(x<sub>n-1</sub>) = ${fmtVal(row.f0)}, f(x<sub>n</sub>) = ${fmtVal(row.f1)}</div>
          <div><strong>Secant Intercept:</strong> x<sub>n+1</sub> = <strong>${fmtVal(row.p2)}</strong> &bull; f(x<sub>n+1</sub>) = ${fmtVal(row.fVal)}</div>
          <div><strong>Absolute Error E<sub>a</sub>:</strong> ${fmtError(row.ea)} &bull; <strong>Relative Error &epsilon;<sub>a</sub>:</strong> ${fmtRelError(row.relEa)}</div>
        `;
      } else if (state.method === 'newton') {
        mathHtml = `
          <div><strong>Current Estimate:</strong> x<sub>n</sub> = <strong>${fmtVal(row.xn)}</strong></div>
          <div><strong>Curve Value:</strong> f(x<sub>n</sub>) = ${fmtVal(row.fVal)} &bull; <strong>Derivative:</strong> f'(x<sub>n</sub>) = ${fmtVal(row.dfVal)}</div>
          <div><strong>Newton Step:</strong> &minus;f / f' = ${fmtVal(-row.fVal / row.dfVal)}</div>
          <div><strong>Absolute Error E<sub>a</sub>:</strong> ${fmtError(row.ea)} &bull; <strong>Relative Error &epsilon;<sub>a</sub>:</strong> ${fmtRelError(row.relEa)}</div>
        `;
      }

      if (el.stepDetailMath) el.stepDetailMath.innerHTML = mathHtml;
    }

    function play() {
      if (state.playback.isPlaying) return;
      if (!state.currentResult || state.currentResult.rows.length === 0) return;
      const total = state.currentResult.rows.length;
      if (state.playback.currentStep >= total) {
        setStep(1, false);
      }
      state.playback.isPlaying = true;
      if (el.iconPlay) el.iconPlay.style.display = 'none';
      if (el.iconPause) el.iconPause.style.display = 'block';
      if (el.fsIconPlay) el.fsIconPlay.style.display = 'none';
      if (el.fsIconPause) el.fsIconPause.style.display = 'block';

      state.playback.timer = setInterval(() => {
        if (!state.currentResult) {
          pause();
          return;
        }
        const total = state.currentResult.rows.length;
        if (state.playback.currentStep >= total) {
          pause();
          return;
        }
        setStep(state.playback.currentStep + 1, true);
      }, state.playback.speed);
    }

    function pause() {
      state.playback.isPlaying = false;
      if (state.playback.timer) {
        clearInterval(state.playback.timer);
        state.playback.timer = null;
      }
      if (el.iconPlay) el.iconPlay.style.display = 'block';
      if (el.iconPause) el.iconPause.style.display = 'none';
      if (el.fsIconPlay) el.fsIconPlay.style.display = 'block';
      if (el.fsIconPause) el.fsIconPause.style.display = 'none';
    }

    // Attach Play/Pause listeners
    if (el.btnPlayPause) {
      el.btnPlayPause.addEventListener('click', () => {
        if (state.playback.isPlaying) pause();
        else play();
      });
    }
    if (el.fsBtnPlayPause) {
      el.fsBtnPlayPause.addEventListener('click', () => {
        if (state.playback.isPlaying) pause();
        else play();
      });
    }

    // Step Navigation Listeners (Main Bar)
    if (el.btnStepFirst) el.btnStepFirst.addEventListener('click', () => { pause(); setStep(1, true); });
    if (el.btnStepPrev) el.btnStepPrev.addEventListener('click', () => { pause(); setStep(state.playback.currentStep - 1, true); });
    if (el.btnStepNext) el.btnStepNext.addEventListener('click', () => { pause(); setStep(state.playback.currentStep + 1, true); });
    if (el.btnStepLast) el.btnStepLast.addEventListener('click', () => {
      pause();
      if (state.currentResult) setStep(state.currentResult.rows.length, true);
    });

    // Step Navigation Listeners (Fullscreen HUD)
    if (el.fsBtnStepFirst) el.fsBtnStepFirst.addEventListener('click', () => { pause(); setStep(1, true); });
    if (el.fsBtnStepPrev) el.fsBtnStepPrev.addEventListener('click', () => { pause(); setStep(state.playback.currentStep - 1, true); });
    if (el.fsBtnStepNext) el.fsBtnStepNext.addEventListener('click', () => { pause(); setStep(state.playback.currentStep + 1, true); });
    if (el.fsBtnStepLast) el.fsBtnStepLast.addEventListener('click', () => {
      pause();
      if (state.currentResult) setStep(state.currentResult.rows.length, true);
    });

    // Slider Scrubber Inputs
    if (el.stepSlider) {
      el.stepSlider.addEventListener('input', (e) => {
        pause();
        setStep(parseInt(e.target.value, 10), false);
      });
    }
    if (el.fsStepSlider) {
      el.fsStepSlider.addEventListener('input', (e) => {
        pause();
        setStep(parseInt(e.target.value, 10), false);
      });
    }

    state.setStep = setStep;
    state.pausePlayback = pause;
  }

  /* ==========================================================================
     8B. FULLSCREEN GRAPH CONTROLLER
     ========================================================================== */

  function initFullscreenGraph() {
    const panel = el.geomCanvasPanel;
    if (!panel) return;

    function toggleFullscreen() {
      const isFs = document.fullscreenElement === panel || panel.classList.contains('is-fullscreen');

      if (!isFs) {
        if (panel.requestFullscreen) {
          panel.requestFullscreen().catch(() => {
            panel.classList.add('is-fullscreen');
            handleFullscreenStatus(true);
          });
        } else {
          panel.classList.add('is-fullscreen');
          handleFullscreenStatus(true);
        }
      } else {
        if (document.fullscreenElement) {
          document.exitFullscreen().catch(() => {});
        }
        panel.classList.remove('is-fullscreen');
        handleFullscreenStatus(false);
      }
    }

    function handleFullscreenStatus(isFs) {
      if (el.iconEnterFullscreen) el.iconEnterFullscreen.style.display = isFs ? 'none' : 'inline-block';
      if (el.iconExitFullscreen) el.iconExitFullscreen.style.display = isFs ? 'inline-block' : 'none';

      // Re-render canvas with exact viewport geometry
      setTimeout(() => {
        renderGeomCanvas();
      }, 70);
    }

    document.addEventListener('fullscreenchange', () => {
      const isFs = document.fullscreenElement === panel;
      handleFullscreenStatus(isFs);
    });

    window.addEventListener('keydown', (e) => {
      if (e.key === 'Escape' && panel.classList.contains('is-fullscreen')) {
        panel.classList.remove('is-fullscreen');
        handleFullscreenStatus(false);
      }
    });

    if (el.btnGeomFullscreen) {
      el.btnGeomFullscreen.addEventListener('click', toggleFullscreen);
    }
    if (el.btnFsExit) {
      el.btnFsExit.addEventListener('click', toggleFullscreen);
    }
  }

  /* ==========================================================================
     8C. SCREEN WIDTH SPAN CONTROLLER
     ========================================================================== */

  function initSpanScreenWidth() {
    if (!el.btnSpanScreen || !el.appLayout) return;

    function toggleSpanWidth() {
      const isSpanned = el.appLayout.classList.toggle('span-screen-width');
      el.btnSpanScreen.classList.toggle('active', isSpanned);
      el.btnSpanScreen.title = isSpanned
        ? 'Restore normal layout (Show sidebar)'
        : 'Span graph window across entire screen width (Collapse sidebar)';
      if (el.iconSpanExpand) el.iconSpanExpand.style.display = isSpanned ? 'none' : 'inline-block';
      if (el.iconSpanCollapse) el.iconSpanCollapse.style.display = isSpanned ? 'inline-block' : 'none';

      // Re-render canvas geometry after layout transition
      setTimeout(() => {
        renderCanvases();
      }, 50);
      setTimeout(() => {
        renderCanvases();
      }, 260);
    }

    el.btnSpanScreen.addEventListener('click', toggleSpanWidth);

    // Keyboard shortcut 'w' or 'W' (when not focused in an input/select)
    window.addEventListener('keydown', (e) => {
      if ((e.key === 'w' || e.key === 'W') && !['INPUT', 'SELECT', 'TEXTAREA'].includes(e.target.tagName)) {
        toggleSpanWidth();
      }
    });

    // Auto-resize with ResizeObserver for ultra-smooth responsiveness
    if (window.ResizeObserver && el.geomCanvasWrapper) {
      const ro = new ResizeObserver(() => {
        renderCanvases();
      });
      ro.observe(el.geomCanvasWrapper);
    }
  }

  /* ==========================================================================
     9. TABULAR DATA RENDERING & EXPORT
     ========================================================================== */

  function renderTable(result, method) {
    if (!result || !result.rows) {
      el.iterationTableHead.innerHTML = '';
      el.iterationTableBody.innerHTML = '';
      el.tableRowCount.textContent = '0';
      return;
    }

    el.tableRowCount.textContent = result.rows.length.toString();

    let theadHtml = '';
    if (method === 'bisection') {
      theadHtml = `
        <tr>
          <th>Iter (k)</th>
          <th>Lower Bound (a)</th>
          <th>Upper Bound (b)</th>
          <th>Midpoint (c)</th>
          <th>f(c)</th>
          <th>Abs Error (E<sub>a</sub>)</th>
          <th>Rel Error (&epsilon;<sub>a</sub>)</th>
        </tr>
      `;
    } else if (method === 'secant') {
      theadHtml = `
        <tr>
          <th>Iter (k)</th>
          <th>x<sub>n-1</sub></th>
          <th>x<sub>n</sub></th>
          <th>x<sub>n+1</sub></th>
          <th>f(x<sub>n+1</sub>)</th>
          <th>Abs Error (E<sub>a</sub>)</th>
          <th>Rel Error (&epsilon;<sub>a</sub>)</th>
        </tr>
      `;
    } else if (method === 'newton') {
      theadHtml = `
        <tr>
          <th>Iter (k)</th>
          <th>x<sub>n</sub></th>
          <th>f(x<sub>n</sub>)</th>
          <th>f'(x<sub>n</sub>)</th>
          <th>Abs Error (E<sub>a</sub>)</th>
          <th>Rel Error (&epsilon;<sub>a</sub>)</th>
        </tr>
      `;
    }
    el.iterationTableHead.innerHTML = theadHtml;

    let tbodyHtml = '';
    const n = result.rows.length;

    result.rows.forEach((r, idx) => {
      const isLast = idx === n - 1;
      const stopTag = isLast && result.converged ? `<span class="stop-tag">&larr; [STOP]</span>` : '';

      if (method === 'bisection') {
        tbodyHtml += `
          <tr data-iter="${r.iteration}" class="${isLast ? 'row-stopped' : ''}">
            <td>${r.iteration}${stopTag}</td>
            <td class="font-mono">${fmtVal(r.a, 8)}</td>
            <td class="font-mono">${fmtVal(r.b, 8)}</td>
            <td class="font-mono font-bold">${fmtVal(r.c, 8)}</td>
            <td class="font-mono">${fmtVal(r.fVal, 6)}</td>
            <td class="font-mono">${fmtError(r.ea)}</td>
            <td class="font-mono">${fmtRelError(r.relEa)}</td>
          </tr>
        `;
      } else if (method === 'secant') {
        tbodyHtml += `
          <tr data-iter="${r.iteration}" class="${isLast ? 'row-stopped' : ''}">
            <td>${r.iteration}${stopTag}</td>
            <td class="font-mono">${fmtVal(r.p0, 8)}</td>
            <td class="font-mono">${fmtVal(r.p1, 8)}</td>
            <td class="font-mono font-bold">${fmtVal(r.p2, 8)}</td>
            <td class="font-mono">${fmtVal(r.fVal, 6)}</td>
            <td class="font-mono">${fmtError(r.ea)}</td>
            <td class="font-mono">${fmtRelError(r.relEa)}</td>
          </tr>
        `;
      } else if (method === 'newton') {
        tbodyHtml += `
          <tr data-iter="${r.iteration}" class="${isLast ? 'row-stopped' : ''}">
            <td>${r.iteration}${stopTag}</td>
            <td class="font-mono font-bold">${fmtVal(r.xn, 8)}</td>
            <td class="font-mono">${fmtVal(r.fVal, 6)}</td>
            <td class="font-mono">${fmtVal(r.dfVal, 6)}</td>
            <td class="font-mono">${fmtError(r.ea)}</td>
            <td class="font-mono">${fmtRelError(r.relEa)}</td>
          </tr>
        `;
      }
    });

    el.iterationTableBody.innerHTML = tbodyHtml;

    // Attach row click to jump to step
    const rows = el.iterationTableBody.querySelectorAll('tr');
    rows.forEach(tr => {
      tr.addEventListener('click', () => {
        const iter = parseInt(tr.dataset.iter, 10);
        state.setStep(iter);
      });
    });
  }

  function highlightTableRow(step) {
    const rows = el.iterationTableBody.querySelectorAll('tr');
    rows.forEach(tr => {
      const iter = parseInt(tr.dataset.iter, 10);
      tr.classList.toggle('row-active', iter === step);
    });
  }

  function initTableExport() {
    el.btnCopyTable.addEventListener('click', () => {
      const table = document.getElementById('iteration-table');
      if (!table) return;
      let text = '';
      table.querySelectorAll('tr').forEach(row => {
        const cols = Array.from(row.querySelectorAll('th, td')).map(td => td.innerText.replace(/\s+/g, ' ').trim());
        text += cols.join('\t') + '\n';
      });

      navigator.clipboard.writeText(text).then(() => {
        const orig = el.btnCopyTable.innerHTML;
        el.btnCopyTable.innerHTML = `<span>Copied! &check;</span>`;
        setTimeout(() => { el.btnCopyTable.innerHTML = orig; }, 1800);
      });
    });

    el.btnExportCsv.addEventListener('click', () => {
      const result = state.currentResult;
      if (!result || !result.rows || result.rows.length === 0) return;

      let csv = '';
      if (state.method === 'bisection') {
        csv += 'Iteration,Lower_a,Upper_b,Midpoint_c,f_c,Abs_Error_Ea,Rel_Error_ea_pct\n';
        result.rows.forEach(r => {
          csv += `${r.iteration},${r.a},${r.b},${r.c},${r.fVal},${r.ea ?? ''},${r.relEa !== null ? r.relEa * 100 : ''}\n`;
        });
      } else if (state.method === 'secant') {
        csv += 'Iteration,x_prev,x_curr,x_next,f_next,Abs_Error_Ea,Rel_Error_ea_pct\n';
        result.rows.forEach(r => {
          csv += `${r.iteration},${r.p0},${r.p1},${r.p2},${r.fVal},${r.ea ?? ''},${r.relEa !== null ? r.relEa * 100 : ''}\n`;
        });
      } else if (state.method === 'newton') {
        csv += 'Iteration,x_curr,f_x,df_x,Abs_Error_Ea,Rel_Error_ea_pct\n';
        result.rows.forEach(r => {
          csv += `${r.iteration},${r.xn},${r.fVal},${r.dfVal},${r.ea ?? ''},${r.relEa !== null ? r.relEa * 100 : ''}\n`;
        });
      }

      const blob = new Blob([csv], { type: 'text/csv;charset=utf-8;' });
      const url = URL.createObjectURL(blob);
      const a = document.createElement('a');
      a.href = url;
      a.download = `rootfinder_${state.method}_results.csv`;
      a.click();
      URL.revokeObjectURL(url);
    });
  }

  /* ==========================================================================
     10. METHOD EXECUTION & SHOOTOUT ENGINE
     ========================================================================== */

  function updateMetrics(result) {
    if (!result || !result.converged || result.rows.length === 0) {
      el.metricRoot.textContent = '—';
      el.metricFVal.textContent = 'f(x*) = —';
      el.metricIters.textContent = '—';
      el.metricMethodName.textContent = `Method: ${state.method}`;
      el.metricRelEa.textContent = '—';
      el.metricAbsEa.textContent = 'E_a = —';
      el.metricExactErr.textContent = '—';
      el.metricExactRel.textContent = 'ε_t = —';
      return;
    }

    const lastRow = result.rows[result.rows.length - 1];
    const root = result.root;

    el.metricRoot.textContent = fmtVal(root, 8);
    try {
      el.metricFVal.textContent = `f(x*) = ${fmtVal(state.f(root), 6)}`;
    } catch (e) {
      el.metricFVal.textContent = `f(x*) = —`;
    }

    el.metricIters.textContent = result.iterations.toString();
    el.metricMethodName.textContent = `Method: ${state.method.toUpperCase()}`;

    el.metricRelEa.textContent = fmtRelError(lastRow.relEa);
    el.metricAbsEa.textContent = `E_a = ${fmtError(lastRow.ea)}`;

    // Exact theoretical errors if true root is supplied
    if (state.trueRoot !== null && isFinite(state.trueRoot)) {
      const et = Math.abs(state.trueRoot - root);
      const relEt = Math.abs(state.trueRoot) > 1e-15 ? (et / Math.abs(state.trueRoot)) : 0;
      el.metricExactErr.textContent = fmtError(et);
      el.metricExactRel.textContent = `ε_t = ${(relEt * 100).toFixed(4)}%`;
    } else {
      el.metricExactErr.textContent = 'N/A';
      el.metricExactRel.textContent = 'Enter true root';
    }
  }

  function runSolver() {
    el.solverAlert.style.display = 'none';

    // 1. Compile math expression
    const rawFunc = el.inputFunc.value.trim();
    if (!rawFunc) {
      showAlert("Please enter a mathematical function f(x).");
      return;
    }

    try {
      state.f = MathParser.compile(rawFunc);
      // Verify evaluation at test points
      let valid = false;
      for (const tx of [1.0, 0.5, 2.0, 0.0, -1.0]) {
        try {
          const v = state.f(tx);
          if (isFinite(v)) { valid = true; break; }
        } catch (e) {}
      }
      if (!valid) throw new Error("Function returned invalid values or could not be evaluated.");
    } catch (err) {
      showAlert(`Syntax / Math Error: ${err.message}`);
      return;
    }

    // 2. Parse Tolerance and Max Iterations
    state.tol = parseTolerance(el.inputTol.value);
    state.maxIter = parseInt(el.inputMaxIter.value, 10) || 100;

    // 3. Parse optional true root
    const trueRootStr = el.inputTrueRoot.value.trim();
    if (trueRootStr) {
      try {
        const rootFn = MathParser.compile(trueRootStr);
        state.trueRoot = rootFn(0);
      } catch (e) {
        state.trueRoot = parseFloat(trueRootStr);
      }
    } else {
      state.trueRoot = null;
    }

    // 4. Solve according to active method
    let result = null;
    if (state.method === 'bisection') {
      const a = parseFloat(el.inputBisA.value);
      const b = parseFloat(el.inputBisB.value);
      if (isNaN(a) || isNaN(b)) {
        showAlert("Please enter valid numbers for lower bound (a) and upper bound (b).");
        return;
      }
      result = solveBisection(state.f, a, b, state.tol, state.maxIter);
      if (result.errorMsg) {
        showAlert(result.errorMsg);
      }
    } else if (state.method === 'secant') {
      const x0 = parseFloat(el.inputSecX0.value);
      const x1 = parseFloat(el.inputSecX1.value);
      if (isNaN(x0) || isNaN(x1)) {
        showAlert("Please enter valid numbers for initial guesses x₀ and x₁.");
        return;
      }
      result = solveSecant(state.f, x0, x1, state.tol, state.maxIter);
      if (result.errorMsg) {
        showAlert(result.errorMsg);
      }
    } else if (state.method === 'newton') {
      const x0 = parseFloat(el.inputNewtonX0.value);
      if (isNaN(x0)) {
        showAlert("Please enter a valid number for initial guess x₀.");
        return;
      }
      result = solveNewton(state.f, x0, state.tol, state.maxIter);
      if (result.errorMsg) {
        showAlert(result.errorMsg);
      }
    }

    state.currentResult = result;

    if (result && result.rows.length > 0) {
      // Setup Scrubber slider
      if (el.stepSlider) {
        el.stepSlider.min = 1;
        el.stepSlider.max = result.rows.length;
        el.stepSlider.value = 1;
      }
      if (el.fsStepSlider) {
        el.fsStepSlider.min = 1;
        el.fsStepSlider.max = result.rows.length;
        el.fsStepSlider.value = 1;
      }
      state.playback.currentStep = 1;

      // Update Viewport bounds to frame the root nicely
      let fitA = 0, fitB = 1;
      if (state.method === 'bisection') {
        fitA = parseFloat(el.inputBisA.value);
        fitB = parseFloat(el.inputBisB.value);
      } else if (state.method === 'secant') {
        fitA = parseFloat(el.inputSecX0.value);
        fitB = parseFloat(el.inputSecX1.value);
      } else {
        fitA = parseFloat(el.inputNewtonX0.value) - 1;
        fitB = parseFloat(el.inputNewtonX0.value) + 1;
      }
      state.autoFitView(fitA, fitB, result.root);

      updateMetrics(result);
      renderTable(result, state.method);
      state.setStep(1, false);
    }
  }

  function runShootout() {
    if (!state.f) runSolver();
    if (!state.f) return;

    // Switch to Shootout view tab
    switchView('shootout');

    // Run all 3
    const a = parseFloat(el.inputBisA.value) || 0;
    const b = parseFloat(el.inputBisB.value) || 1;
    const x0 = parseFloat(el.inputSecX0.value) || a;
    const x1 = parseFloat(el.inputSecX1.value) || b;
    const n0 = parseFloat(el.inputNewtonX0.value) || (a + b) / 2;

    const resBis = solveBisection(state.f, a, b, state.tol, state.maxIter);
    const resSec = solveSecant(state.f, x0, x1, state.tol, state.maxIter);
    const resNew = solveNewton(state.f, n0, state.tol, state.maxIter);

    state.shootoutResults = {
      bisection: resBis,
      secant: resSec,
      newton: resNew
    };

    // Populate comparison table
    const methodsMeta = [
      { name: 'Bisection Method', res: resBis, rate: 'Linear (p = 1.0)', inputs: `[${fmtVal(a)}, ${fmtVal(b)}]` },
      { name: 'Secant Method', res: resSec, rate: 'Superlinear (p ≈ 1.62)', inputs: `x₀=${fmtVal(x0)}, x₁=${fmtVal(x1)}` },
      { name: 'Newton-Raphson', res: resNew, rate: 'Quadratic (p = 2.0)', inputs: `x₀=${fmtVal(n0)}` }
    ];

    let html = '';
    methodsMeta.forEach(m => {
      const ok = m.res && m.res.converged;
      const iters = m.res ? m.res.iterations : 'Failed';
      const root = m.res && m.res.root !== null ? fmtVal(m.res.root, 8) : '—';
      const lastErr = m.res && m.res.rows.length > 0 ? fmtRelError(m.res.rows[m.res.rows.length - 1].relEa) : '—';
      const statusBadge = ok ?
        '<span class="badge" style="background:var(--success-subtle);color:var(--success)">Converged &check;</span>' :
        '<span class="badge" style="background:var(--danger-subtle);color:var(--danger)">Error / Diverged</span>';

      html += `
        <tr>
          <td><strong>${m.name}</strong></td>
          <td>${m.inputs}</td>
          <td>${iters}</td>
          <td>${root}</td>
          <td>${lastErr}</td>
          <td>${m.rate}</td>
          <td>${statusBadge}</td>
        </tr>
      `;
    });

    el.shootoutTableBody.innerHTML = html;
    renderShootoutCanvas();
  }

  function showAlert(msg) {
    el.solverAlert.style.display = 'flex';
    el.solverAlert.className = 'alert-box danger';
    el.solverAlert.innerHTML = `
      <svg width="18" height="18" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2">
        <circle cx="12" cy="12" r="10"/>
        <line x1="12" y1="8" x2="12" y2="12"/>
        <line x1="12" y1="16" x2="12.01" y2="16"/>
      </svg>
      <div>${msg}</div>
    `;
  }

  /* ==========================================================================
     11. AUTO-SCANNER & PRESETS INTEGRATION
     ========================================================================== */

  function initScanner() {
    el.btnScanRoots.addEventListener('click', () => {
      const raw = el.inputFunc.value.trim();
      if (!raw) return;

      try {
        const fn = MathParser.compile(raw);
        const brackets = findBrackets(fn, -10, 10, 200);

        if (brackets.length === 0) {
          el.scannerSummaryText.textContent = 'No sign-change brackets found in [-10, 10]. Function might have only complex roots or no zeros.';
          el.bracketPillsList.innerHTML = '<span class="text-muted text-sm">Try widening function range or inspecting curve.</span>';
        } else {
          el.scannerSummaryText.textContent = `Discovered ${brackets.length} root bracket(s) in domain [-10, 10]:`;
          let html = '';
          brackets.forEach(b => {
            html += `<button type="button" class="bracket-pill" data-a="${b.a}" data-b="${b.b}">[${b.a}, ${b.b}]</button>`;
          });
          el.bracketPillsList.innerHTML = html;

          // Auto-apply the first or smallest positive bracket
          const pos = brackets.find(b => (b.a + b.b) / 2 >= 0) || brackets[0];
          applyBracket(pos.a, pos.b);
        }

        el.scannerCard.style.display = 'block';

        // Add pill click events
        el.bracketPillsList.querySelectorAll('.bracket-pill').forEach(pill => {
          pill.addEventListener('click', () => {
            const a = parseFloat(pill.dataset.a);
            const b = parseFloat(pill.dataset.b);
            applyBracket(a, b);
          });
        });
      } catch (err) {
        showAlert(`Scanner Error: ${err.message}`);
      }
    });

    el.btnCloseScanner.addEventListener('click', () => {
      el.scannerCard.style.display = 'none';
    });

    function applyBracket(a, b) {
      el.inputBisA.value = a;
      el.inputBisB.value = b;
      el.inputSecX0.value = a;
      el.inputSecX1.value = b;
      el.inputNewtonX0.value = cleanNum((a + b) / 2);
      checkBolzanoBadge();
      runSolver();
    }
  }

  function checkBolzanoBadge() {
    if (state.method !== 'bisection') return;
    const a = parseFloat(el.inputBisA.value);
    const b = parseFloat(el.inputBisB.value);
    if (isNaN(a) || isNaN(b) || !state.f) {
      el.bolzanoBadge.style.display = 'none';
      return;
    }

    try {
      const fa = state.f(a);
      const fb = state.f(b);
      el.bolzanoBadge.style.display = 'flex';
      if (fa * fb <= 0) {
        el.bolzanoBadge.className = 'badge-status success';
        el.bolzanoBadge.innerHTML = `<span>Bolzano: f(${a})=${fmtVal(fa)}, f(${b})=${fmtVal(fb)} (Sign Change &check;)</span>`;
      } else {
        el.bolzanoBadge.className = 'badge-status danger';
        el.bolzanoBadge.innerHTML = `<span>Bolzano Violated: f(${a})=${fmtVal(fa)} and f(${b})=${fmtVal(fb)} have the SAME sign!</span>`;
      }
    } catch (e) {
      el.bolzanoBadge.style.display = 'none';
    }
  }

  const PRESETS = {
    cubic: {
      func: 'x^3 - 7x + 1',
      a: 0, b: 1, x0: 0, x1: 1, n0: 0.5,
      trueRoot: '0.14373989',
      tol: '0.0001%'
    },
    transcendental: {
      func: 'x^3 - x - 11',
      a: 2, b: 3, x0: 2, x1: 3, n0: 2.5,
      trueRoot: '2.3736498',
      tol: '0.05%'
    },
    trig: {
      func: 'cos(x) - x',
      a: 0, b: 1, x0: 0, x1: 1, n0: 0.5,
      trueRoot: '0.739085133',
      tol: '0.0001%'
    },
    exp: {
      func: 'e^x - 3x',
      a: 0, b: 1, x0: 0, x1: 1, n0: 0.6,
      trueRoot: '0.61906129',
      tol: '0.0001%'
    },
    poly4: {
      func: 'x^4 - 2x^3 - 4x^2 + 4x + 4',
      a: 0, b: 2, x0: 0, x1: 2, n0: 1.5,
      trueRoot: '1.41421356',
      tol: '0.0001%'
    },
    sqrt2: {
      func: 'x^2 - 2',
      a: 1, b: 2, x0: 1, x1: 2, n0: 1.5,
      trueRoot: '1.41421356',
      tol: '0.0001%'
    }
  };

  function initPresets() {
    el.presetSelect.addEventListener('change', (e) => {
      const key = e.target.value;
      if (!key || !PRESETS[key]) return;
      const p = PRESETS[key];
      el.inputFunc.value = p.func;
      el.inputBisA.value = p.a;
      el.inputBisB.value = p.b;
      el.inputSecX0.value = p.x0;
      el.inputSecX1.value = p.x1;
      el.inputNewtonX0.value = p.n0;
      el.inputTol.value = p.tol;
      el.inputTrueRoot.value = p.trueRoot;

      updateFuncPreview();
      runSolver();
    });
  }

  function updateFuncPreview() {
    const raw = el.inputFunc.value.trim();
    if (!raw) {
      el.funcPreview.textContent = 'Preview: —';
      return;
    }
    // Simple math formatting for preview banner
    let preview = raw
      .replace(/\^3/g, '³')
      .replace(/\^2/g, '²')
      .replace(/\^4/g, '⁴')
      .replace(/\*/g, '&middot;');
    el.funcPreview.innerHTML = `Preview: <em>f(x) = ${preview}</em>`;
    checkBolzanoBadge();
  }

  /* ==========================================================================
     12. VIEW SWITCHER & MODAL HANDLER
     ========================================================================== */

  function switchView(viewName) {
    el.viewTabs.forEach(tab => {
      const active = tab.dataset.view === viewName;
      tab.classList.toggle('active', active);
      tab.setAttribute('aria-selected', active ? 'true' : 'false');
    });

    Object.keys(el.viewPanels).forEach(key => {
      const panel = el.viewPanels[key];
      if (panel) {
        panel.style.display = key === viewName ? 'block' : 'none';
      }
    });

    // Toolbar visibility for table
    el.tableActionsToolbar.style.display = viewName === 'table' ? 'flex' : 'none';

    if (viewName === 'vis') {
      renderCanvases();
    } else if (viewName === 'shootout') {
      renderShootoutCanvas();
    }
  }

  function initTabs() {
    // Method selector tabs
    el.methodTabs.forEach(tab => {
      tab.addEventListener('click', () => {
        el.methodTabs.forEach(t => {
          t.classList.remove('active');
          t.setAttribute('aria-selected', 'false');
        });
        tab.classList.add('active');
        tab.setAttribute('aria-selected', 'true');

        const m = tab.dataset.method;
        state.method = m;

        el.inputsBisection.style.display = m === 'bisection' ? 'block' : 'none';
        el.inputsSecant.style.display = m === 'secant' ? 'block' : 'none';
        el.inputsNewton.style.display = m === 'newton' ? 'block' : 'none';

        checkBolzanoBadge();
        runSolver();
      });
    });

    // Workspace view tabs
    el.viewTabs.forEach(tab => {
      tab.addEventListener('click', () => {
        switchView(tab.dataset.view);
      });
    });

    // Theory dialog tabs
    el.theoryTabs.forEach(tab => {
      tab.addEventListener('click', () => {
        el.theoryTabs.forEach(t => t.classList.remove('active'));
        tab.classList.add('active');
        const targetId = tab.dataset.theoryTab;
        el.theoryContents.forEach(c => {
          c.style.display = c.id === targetId ? 'block' : 'none';
        });
      });
    });

    // Theory modal open/close
    el.btnOpenTheory.addEventListener('click', () => {
      if (typeof el.theoryDialog.showModal === 'function') {
        el.theoryDialog.showModal();
      } else {
        el.theoryDialog.setAttribute('open', '');
      }
    });

    el.btnCloseTheory.addEventListener('click', () => el.theoryDialog.close());
    el.btnTheoryDone.addEventListener('click', () => el.theoryDialog.close());
    el.theoryDialog.addEventListener('click', (e) => {
      if (e.target === el.theoryDialog) el.theoryDialog.close();
    });
  }

  /* ==========================================================================
     13. INITIALIZATION
     ========================================================================== */

  function init() {
    initTheme();
    initTabs();
    initCanvasInteractions();
    initPlayback();
    initFullscreenGraph();
    initSpanScreenWidth();
    initTableExport();
    initScanner();
    initPresets();

    el.inputFunc.addEventListener('input', updateFuncPreview);
    el.inputBisA.addEventListener('input', checkBolzanoBadge);
    el.inputBisB.addEventListener('input', checkBolzanoBadge);

    el.form.addEventListener('submit', (e) => {
      e.preventDefault();
      runSolver();
    });

    el.btnCompareAll.addEventListener('click', () => {
      runShootout();
    });

    window.addEventListener('resize', () => {
      renderCanvases();
    });

    // Initial solve with default equation
    updateFuncPreview();
    runSolver();
  }

  // Run on DOM ready
  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', init);
  } else {
    init();
  }

})();
