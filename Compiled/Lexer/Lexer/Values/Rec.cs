
using System;

public class Rec : Val
{

    public string x;

    public Val v;

    public Rec()
    {
        this.x = "";
        this.v = null;
    }

    public Rec(string x, Val v)
    {
        this.x = x;
        this.v = v;
    }
}
