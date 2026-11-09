object Animal
object Dog extends Animal

method main {
    val perro = new Dog;
    println("Tipo real: ${perro.getClass.getSimpleName}")
}