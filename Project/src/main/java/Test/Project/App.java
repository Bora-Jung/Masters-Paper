package Test.Project;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;

/**
 * Hello world!
 *
 */
public class App 
{
    public static void main( String[] args )
    {
    	 String csvFile = "D:/list.csv";
         BufferedReader br = null;
         BufferedWriter bw = null;
         String line = "";
         String cvsSplitBy = ",";

         try {

             br = new BufferedReader(new FileReader(csvFile));
             bw = new BufferedWriter(new FileWriter("D:/지수data.csv"));
             br.readLine();
             while ((line = br.readLine()) != null) {

                 // use comma as separator
                 String[] data = line.split(cvsSplitBy);

                 String disease[]=data[1].split("/");
                 
                 for(int i=0;i<disease.length;i++)
                 {
                	 bw.write(data[0]+","+disease[i]+","+(i+1));
                	 bw.newLine();
                 }
                              }
             bw.flush();
             bw.close();
             br.close();

         } catch (FileNotFoundException e) {
             e.printStackTrace();
         } catch (IOException e) {
             e.printStackTrace();
         } finally {
             if (br != null) {
                 try {
                     br.close();
                 } catch (IOException e) {
                     e.printStackTrace();
                 }
             }
         }
    }
}
