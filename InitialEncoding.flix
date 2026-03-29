mod InitialEncoding {

    enum Expr {
        case B(Bool)
        case Add(Expr, Expr)
        case I(Int32)
        // case Lit(Int32)
        case Mult(Expr, Expr)
    }

    def eval(e: Expr): Int32 = match e {
        case Expr.Add(l, r) => eval(l) + eval(r)
        case Expr.I(i)    => i
        case Expr.Mult(l, r) => eval(l) * eval(r)
        case Expr.B(b) => b? // variable hole that must return an Int32
    }

    def print(e: Expr): String = match e {
        case Expr.I(i) => Int32.toString(i)
        case Expr.Add(l, r) => print(l) + "+" + print(r)
        case Expr.Mult(l, r) => print(l) + "*" +  print(r)
        case Expr.B(b) => if (b) "true" else "false"
    }

    pub def main(): Unit \ IO = {
        println(eval(Expr.Add(Expr.I(1), Expr.Mult(Expr.I(2), Expr.I(3)))));
        println(print(Expr.Add(Expr.B(true), Expr.Mult(Expr.I(2), Expr.I(3)))))
    }

}