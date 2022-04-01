public class Pair<T, V>
{ 
    public T first;
    public V second;

    public Pair()
    {
        this.first = default(T);
        this.second = default(V);
    }

    public Pair(T first, V second)
    {
        this.first = first;
        this.second = second;
    }
}