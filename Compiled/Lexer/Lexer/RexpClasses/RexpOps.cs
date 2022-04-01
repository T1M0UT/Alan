
public class RexpOps
{

    // operator |
    public static Alt verticalBarOP(Rexp r, Rexp s)
    {
        return new Alt(r, s);
    }

    // operator %
    public static Star percentOP(Rexp r)
    {
        return new Star(r);
    }

    // operator ~
    public static Seq tildeOP(Rexp r, Rexp s)
    {
        return new Seq(r, s);
    }

    public static Seq powOP(Rexp r, int times)
    {
        if ((times == 2))
        {
            return new Seq(r, r);
        }

        return new Seq(r, RexpOps.powOP(r, (times - 1)));
    }

    public static Alt questionOP(Rexp r)
    {
        return new Alt(new One(), r);
    }
}