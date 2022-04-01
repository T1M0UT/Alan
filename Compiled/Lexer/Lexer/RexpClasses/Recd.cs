using System;
public class Recd : Rexp
{

    public string x;

    public Rexp r;

    public Recd()
    {
        this.x = "";
        this.r = null;
    }

    public Recd(string x, Rexp r)
    {
        this.x = x;
        this.r = r;
    }
}