object Contenedor[T](val contenido: T)

Contenedor.leer = method(): T {
    este.contenido
}

Contenedor.elegir = method(otro: T): T {
    otro
}

method main {
    val numero = new Contenedor[Int](7);
    val texto = new Contenedor[String]("hola");
    "${texto.leer()} ${numero.leer() + 1} ${numero.elegir(9)}"
}