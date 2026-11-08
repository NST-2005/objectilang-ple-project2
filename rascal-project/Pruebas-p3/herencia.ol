object Hija extends Base(val extra: Int)

method main {
    val hija = new Hija(7, 5);
    hija.valor + hija.extra
}

object Base(val valor: Int)