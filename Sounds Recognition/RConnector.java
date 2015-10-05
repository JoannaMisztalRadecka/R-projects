/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package recognition;

import java.util.logging.Level;
import java.util.logging.Logger;
import org.rosuda.REngine.REXP;
import org.rosuda.REngine.Rserve.RConnection;
import org.rosuda.REngine.Rserve.RserveException;

/**
 *
 * @author asia
 */
public  class RConnector {

     public void processWav(String path,String name) {
        try {
            RConnection c = new RConnection();
            c.eval("setwd('" + path + "')");
            c.eval("source('sounds.R')");
            c.eval("processWav('" + name + ".wav','" + name + ".txt')");
            c.close();
        } catch (RserveException ex) {
            Logger.getLogger(recorder.class.getName()).log(Level.SEVERE, null, ex);
        }
    }
         public static String predict(String[] labels,String file, RConnection c) {
        REXP r = null;
        try {
            c.eval("r<-readDataSpectrum('" + file + "')");
            r = c.eval("prediction(x,r)");
            c.close();
            return labels[r.asInteger() - 1];
        } catch (Exception e) {
            System.err.println("ERROR: In prediction " + file);
            System.err.println("The Exception is: " + e.getMessage());
            e.printStackTrace();
        }
        return null;
    }

}
