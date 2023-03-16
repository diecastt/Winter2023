package main.java;

public class HashNode {
    private int key;
    private int value;

    HashNode(int key, int value) {
        this.key = key;
        this.value = value;
    }

    public int getValue() {
        return this.value;
    }

    public void setValue(int value) {
        this.value = value;
    }

    public int getKey() {
        return this.key;
    }
}
