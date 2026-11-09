object Animal
object Dog extends Animal
object List[+A](val item: A)

method main {
    val perros: List[Dog] = new List(new Dog);
    val animales: List[Animal] = perros;
    println(animales.item)
}