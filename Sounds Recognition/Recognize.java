package recognition;

/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
import java.io.BufferedReader;
import java.io.InputStreamReader;
import org.rosuda.REngine.*;
import javax.swing.*;
import org.rosuda.REngine.Rserve.RConnection;


public class Recognize {

    private static String[] labels;
    private String name;
    static JFrame frame;
    String path;
    RConnector rConnector = new RConnector();

    public Recognize(String name,String path) {
        labels = new String[2];
        labels[0] = "tak";
        labels[1] = "nie";
        this.name = name;
        this.path = path;
    }

    public  void initialize(RConnection c) {
        try {
            c.eval("setwd(" + "'/home/asia/Documents/data'" + ")");
            c.eval("source('sounds.R')");
            REXP x = c.eval("x<-initialize()");
        } catch (Exception e) {
            System.out.println("ERROR: In Connection to R ");
            System.out.println("The Exception is " + e.getMessage());
            e.printStackTrace();
        }
    }



    private static void out(String strMessage) {
        System.out.println(strMessage);
    }

   

    public void recognize() {

        BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
        try {
            out("Łączenie z serwerem R...");
            RConnection c = new RConnection();
            out("Tworzenie SOM(trochę potrwa :| )...");
            initialize(c);
            while (true) {
                out("Aby przerwać wybierz 'q',aby nagrywać naciśnij dowolny inny klawisz.");
                if (br.read() == 'q') {
                    break;
                }
               recorder.work(name);
                String pred = RConnector.predict(labels,name+".txt",c);
                out("Powiedziales: " + pred);
            }
            c.close();
        } catch (Exception e) {
            System.err.println("ERROR: In Connection to R ");
            System.err.println("The Exception is " + e.getMessage());
            e.printStackTrace();
        }
    }
}
