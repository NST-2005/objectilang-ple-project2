object Caja(val valor: Int, val etiqueta: String)

method main {
    val caja = new Caja(7, "dato");
    "${caja.etiqueta}: ${caja.valor + 1}"
}