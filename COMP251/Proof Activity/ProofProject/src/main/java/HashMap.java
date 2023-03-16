package main.java;

import java.io.*;
import java.util.*;

public class HashMap {
    public int m; // number of SLOTS AVAILABLE
    public int A; // default random number
    public HashNode[] table;

    HashMap(int A, int seed, int m) {

        this.m = m;

        if (A==-1){
            this.A = generateRandom((int) m,(int) m * power2(6), seed);
        }
        else{
            this.A = A;
        }

        this.table = new HashNode[m];
        for (int i = 0; i < m; i++){
            table[i] = null;
        }

    }

    /** Calculate 2^w*/
    public static int power2(int w) {
        return (int) Math.pow(2, w);
    }

    public static int generateRandom(int min, int max, int seed) {
        Random generator = new Random();
        if(seed>=0){
            generator.setSeed(seed);
        }
        int i = generator.nextInt(max-min-1);
        return i+min+1;
    }

    /** Implements the linear probing hash function */
    public int probe(int key, int i) {
        int h = ((this.A * key) % power2(this.m));
        int hashValue = (h + i) % this.m;
        return hashValue;
    }

    public void insert(HashNode node){
        int i = 0;

        while (i < this.m){
            int hashcode = this.probe(node.getKey(), i);
            if (this.table[hashcode] == null){
                this.table[hashcode] = node;
                return;
            }
            i++;
        }
    }

    public int search(int key){
        for (int i = 0; i < this.m; i++){
            int hashcode = this.probe(key, i);
            if(this.table[hashcode] != null && this.table[hashcode].getKey() == key){
                return this.table[hashcode].getValue();
            }
        }
        return -1;
    }

    public void remove(int key){
        for (int i = 0; i < this.m; i++) {
            int hashcode = this.probe(key, i);
            if (this.table[hashcode] != null && this.table[hashcode].getKey() == key) {
                this.table[hashcode] = null;
                return;
            }
        }
    }

    /**
     * toString method.
     *
     * Provide some visual funcionality to see the elements inside the hash table.
     *
     * @return String
     *  Representation of the hash table in the moment by a string.
     */
    public String toString() {

        String description = "Hash table: [ ";

        for (int i = 0; i < this.m; i++) {

            if (this.table[i] == null) {

                description += "__  ";
            }
            else {

                description += String.format("(%2d, %2d)  ", this.table[i].getKey(), this.table[i].getValue());
            }
        }

        description += "]";

        return description;
    }


}
