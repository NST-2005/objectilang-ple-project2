object Hija extends Base(val extra: Int)

Base.sumar = method(cantidad: Int): Int {
    este.valor + cantidad
}

method main {
    val hija = new Hija(7, 5);
    hija.sumar(hija.extra)
}

object Base(val valor: Int)