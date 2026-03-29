mod InitialEncoding {

    enum Expr {
        case Add(Expr, Expr)
        case Lit(Int32)
    }

    def eval(e: Expr): Int32 = match e {
        case Expr.Add(l, r) => eval(l) + eval(r)
        case Expr.Lit(i)    => i
    }

    pub def main(): Unit \ IO = 
        println(eval(Expr.Add(Expr.Lit(1), Expr.Add(Expr.Lit(2), Expr.Lit(3)))))

}