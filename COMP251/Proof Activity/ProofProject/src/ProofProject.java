package main.java;
import java.util.*;

public class ProofProject {
    public static void main(String[] args) {
        // proof 1: Lookup in a hash table that uses open addressing takes O(1) time.
        ArrayList<Double> successTimes = openAddressing("Successful");
        ArrayList<Double> unsuccessTimes = openAddressing("Unsuccessful");

        for (Double time : successTimes){
            System.out.println(time);
        }


    }

    // running OpenAddressing search operation with successful searches
    public static ArrayList<Double> openAddressing(String searchMode){
        HashMap table = new HashMap(-1, 0, 500);
        ArrayList<Double> times = new ArrayList<Double>();

        for (int i = 0; i < 500; i++){
            table.insert(new HashNode(i, (i * 5) + 30));
        }

        // searching for keys that are not in the table (unsuccessful search)
        if (searchMode == "Unsuccessful"){
            for (int i = 500; i < 1000 ; i++){

                // get the start time
                long start = System.nanoTime();

                table.search(i);

                // get the end time
                double end = System.nanoTime();

                // execution time
                double execution = end - start;

                times.add(execution);
            }
        }
        else if (searchMode == "Successful"){
            // searching for keys that are in the table (successful search)
            for (int i = 0; i < 500; i++) {

                // get the start time
                long start = System.nanoTime();

                table.search(i);

                // get the end time
                double end = System.nanoTime();

                // execution time
                double execution = end - start;

                times.add(execution);
            }
        }
        return times;
    }

}