package com.company;

public class Graph {
    // Number of vertices and number of edges
    public int V, E;
    public Edge[] edges;

    Graph(int V, int E, Edge[] edges){
        this.V = V;
        this.E = E;
        this.edges = edges;
    }

    /**
     * toString method.
     * @return String
     *  Representation of the graph in the moment by a string.
     */
    public String toString() {

        String description = "Graph:\nSource  Weight  Dest\n";

        for (int i = 0; i < this.E; i++) {
                description += String.format("[%d] -- (%d) --> [%d]\n", this.edges[i].src, this.edges[i].weight, this.edges[i].dest);
        }
        return description;
    }

}
