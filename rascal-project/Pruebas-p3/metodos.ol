object Caja(val valor: Int)

Caja.sumar = method(extra: Int): Int {
    este.valor + extra
}

method main {
    val caja = new Caja(7);
    caja.sumar(5)
}