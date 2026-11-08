Caja.calcular = method(extra: Int): Int {
    este.valor + extra
}

object Caja(val valor: Int)

method main {
    val caja = new Caja(7);
    val bolsa = new Bolsa(7);
    "${caja.calcular(5)} y ${bolsa.calcular(5)}"
}

object Bolsa(val valor: Int)

Bolsa.calcular = method(extra: Int): Int {
    este.valor * extra
}
