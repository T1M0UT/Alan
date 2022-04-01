
public class Star : Rexp
{

    public Rexp r;

    public Star()
    {
        this.r = null;
    }

    public Star(Rexp r)
    {
        this.r = r;
    }
}