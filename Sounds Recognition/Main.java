/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package recognition;

/**
 *
 * @author asia
 */
public class Main {

    /**
     * @param args the command line arguments
     */
    public static void main(String[] args) {
        Recognize rec = new Recognize("test","/home/asia/Documents/data/");
        rec.recognize();
    }

}
