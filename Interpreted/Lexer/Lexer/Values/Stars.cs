using System.Collections.Generic;

public class Stars : Val
{

    public List<Val> vs;

    public Stars()
    {
        this.vs = new List<Val>();
    }

    public Stars(List<Val> vs)
    {
        this.vs = vs;
    }
}
