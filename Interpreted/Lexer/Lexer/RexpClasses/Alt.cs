public class Alt : Rexp
{

    public Rexp r1;

    public Rexp r2;

    public Alt()
    {
        this.r1 = null;
        this.r2 = null;
    }

    public Alt(Rexp r1, Rexp r2)
    {
        this.r1 = r1;
        this.r2 = r2;
    }
}