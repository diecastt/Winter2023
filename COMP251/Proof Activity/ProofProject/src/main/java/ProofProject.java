package main.java;
import java.util.*;


public class ProofProject {
    public static void main(String[] args) {

        HashMap table = new HashMap(-1, 0, 500);
        ArrayList<Double> times = new ArrayList<Double>();

        for (int i = 0; i < 500; i++){
            table.insert(new HashNode(i, (i * 5) + 30));
        }

        System.out.println(table);

        // searching for keys that are and aren't in the table
        for (int i = 0; i < 500; i++){

            // get the start time
            long start = System.nanoTime();

            table.search(i);

            // get the end time
            double end = System.nanoTime();

            // execution time
            double execution = end - start;

            times.add(execution);
            System.out.println(execution);
        }
    }
}