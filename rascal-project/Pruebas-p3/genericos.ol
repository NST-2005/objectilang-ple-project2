object Contenedor[T](val contenido: T)

method main {
    val numero = new Contenedor[Int](7);
    val texto: Contenedor[String] = new Contenedor[String]("hola");
    "${texto.contenido} ${numero.contenido + 1}"
}