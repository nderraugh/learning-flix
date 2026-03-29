mod InitialEncoding {

    enum Expr {
        case Add(Expr, Expr)
        case Lit(Int32)
        case Mult(Expr, Expr)
    }

    def eval(e: Expr): Int32 = match e {
        case Expr.Add(l, r) => eval(l) + eval(r)
        case Expr.Lit(i)    => i
        case Expr.Mult(l, r) => eval(l) * eval(r)
    }

    def print(e: Expr): String = match e {
        case Expr.Lit(i) => Int32.toString(i)
        case Expr.Add(l, r) => print(l) + "+" + print(r)
        case Expr.Mult(l, r) => print(l) + "*" +  print(r)
    }

    pub def main(): Unit \ IO = {
        println(eval(Expr.Add(Expr.Lit(1), Expr.Mult(Expr.Lit(2), Expr.Lit(3)))));
        println(print(Expr.Add(Expr.Lit(1), Expr.Mult(Expr.Lit(2), Expr.Lit(3)))))
    }

}