using System;
using System.Collections;

public class Rectification
{

    public static Func<Val, Val> F_ID = (Val v) => v;

    public static Func<Val, Val> F_RIGHT(Func<Val, Val> f) => (Val v) => new Right(f.Invoke(v));

    public static Func<Val, Val> F_LEFT(Func<Val, Val> f) => (Val v) => new Left(f.Invoke(v));

    public static Func<Val, Val> F_ALT(Func<Val, Val> f1, Func<Val, Val> f2) => (Val v) =>
    {
        if ((v is Right))
        {
            return new Right(f2.Invoke(v));
        }
        else if ((v is Left))
        {
            return new Left(f1.Invoke(v));
        }

        return null;

    };


    public static Func<Val, Val> F_SEQ(Func<Val, Val> f1, Func<Val, Val> f2) => (Val v) =>
    {
        if ((v is Sequ))
        {
            return new Sequ(f1.Invoke(((Sequ)(v)).v1), f2.Invoke(((Sequ)(v)).v2));
        }

        return null;

    };


    public static Func<Val, Val> F_SEQ_EMPTY1(Func<Val, Val> f1, Func<Val, Val> f2) => (Val v) =>
    {
        return new Sequ(f1.Invoke(new Empty()), f2.Invoke(v));
    };

    public static Func<Val, Val> F_SEQ_EMPTY2(Func<Val, Val> f1, Func<Val, Val> f2) => (Val v) =>
    {
        return new Sequ(f1.Invoke(v), f2.Invoke(new Empty()));
    };

    public static Func<Val, Val> F_RECD(Func<Val, Val> f) => (Val v) =>
    {
        if ((v is Rec))
        {
            return new Rec(((Rec)(v)).x, f.Invoke(((Rec)(v)).v));
        }

        return null;
    };
    public static Func<Val, Val> F_ERROR()
    {
        throw new Exception("error");
    }
}