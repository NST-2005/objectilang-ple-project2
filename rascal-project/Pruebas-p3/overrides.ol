object Caja(val valor: Int)

Caja.calcular = method(extra: Int): Int { este.valor + extra }
Caja.leer = method(): Int

method main {
    val normal = new Caja(7);
    val especial = new Caja(7) {
        override calcular(extra: Int): Int = este.valor * extra
        override leer(): Int = { este.valor + 2 }
    };
    "${normal.calcular(5)} ${especial.calcular(5)} ${especial.leer()}"
}