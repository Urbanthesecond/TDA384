public class Multithreading {

    public static void main(String[] args) {
        MultithreadThing myThing = new MultithreadThing();
        Thread t1 = new Thread(myThing);

        t1.start();


    }
}
