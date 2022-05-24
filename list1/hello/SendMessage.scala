object SendMessage {
    def main(args:Array[String]): Unit={
        println("Hello, world")
        if(args.length>0)
        {
            println("Hello, " + args(0))
        }
        import java.util.Calendar
        println("Current time is: " + Calendar.getInstance().getTime())
    }
}