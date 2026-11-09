object Animal
object Dog extends Animal
object Puppy extends Dog

object Food
object DogFood extends Food

object Transformer[-In, +Out]
Transformer.transform = method(input: In): Out

method main {
    val animalFeeder: Transformer[Animal, DogFood] = new Transformer[Animal, DogFood] {
        override transform(animal: Animal): DogFood = new DogFood
    };
    val puppyFeeder: Transformer[Puppy, Food] = animalFeeder;
    val comida: Food = puppyFeeder.transform(new Puppy);
    println(comida.getClass.getSimpleName)
}
