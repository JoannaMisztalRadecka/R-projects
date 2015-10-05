package recognition;

/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */



/**
 *
 * @author Asia
 */
/*
 *	SimpleAudioRecorder.java
 *
 *	This file is part of jsresources.org
 */

/*
 * Copyright (c) 1999 - 2003 by Matthias Pfisterer
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * - Redistributions of source code must retain the above copyright notice,
 *   this list of conditions and the following disclaimer.
 * - Redistributions in binary form must reproduce the above copyright
 *   notice, this list of conditions and the following disclaimer in the
 *   documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
 * STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
 * OF THE POSSIBILITY OF SUCH DAMAGE.
 */

/*
|<---            this code is formatted to fit into 80 columns             --->|
*/

import java.io.IOException;
import java.io.File;
import java.io.*;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.sound.sampled.DataLine;
import javax.sound.sampled.TargetDataLine;
import javax.sound.sampled.AudioFormat;
import javax.sound.sampled.AudioSystem;
import javax.sound.sampled.AudioInputStream;
import javax.sound.sampled.LineUnavailableException;
import javax.sound.sampled.AudioFileFormat;
import org.rosuda.REngine.Rserve.RConnection;
import org.rosuda.REngine.Rserve.RserveException;



public class recorder
extends Thread
{
	private TargetDataLine		m_line;
	private AudioFileFormat.Type	m_targetType;
	private AudioInputStream	m_audioInputStream;
	private File			m_outputFile;
        private static RConnector rConnector;



	public recorder(TargetDataLine line, AudioFileFormat.Type targetType, File file)
	{
		m_line = line;
		m_audioInputStream = new AudioInputStream(line);
		m_targetType = targetType;
		m_outputFile = file;
	}

	public void start()
	{
		m_line.start();
		super.start();
	}

	public void stopRecording()
	{
		m_line.stop();
		m_line.close();
	}
	public void run()
	{
			try
			{
				AudioSystem.write(m_audioInputStream, m_targetType, m_outputFile);
			}
			catch (IOException e)
			{
				e.printStackTrace();
			}
	}



	public static void work(String name)
	{
        try {
            BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
            String wavFilename = new String();
            String txtFilename = new String();
            String path = "/home/asia/Documents/data/";
            wavFilename = name + ".wav";
            txtFilename = name + ".txt";
            File outputFile = new File(path + wavFilename);
            AudioFormat audioFormat = new AudioFormat(AudioFormat.Encoding.PCM_SIGNED, 44100.0F, 16, 2, 4, 44100.0F, false);
            DataLine.Info info = new DataLine.Info(TargetDataLine.class, audioFormat);
            TargetDataLine targetDataLine = null;
            try {
                targetDataLine = (TargetDataLine) AudioSystem.getLine(info);
                targetDataLine.open(audioFormat);
            } catch (LineUnavailableException e) {
                out("unable to get a recording line");
                e.printStackTrace();
                System.exit(1);
            }
            AudioFileFormat.Type targetType = AudioFileFormat.Type.WAVE;
            recorder recorder = new recorder(targetDataLine, targetType, outputFile);
            recorder.start();
            out("nagrywanie...");
            out("naciśnij ENTER, by zakończyć nagrywanie.");
            try {
                System.in.read();
            } catch (IOException e) {
                e.printStackTrace();
            }
            recorder.stopRecording();
            out("Zakończono nagrywanie.");
           // rConnector.processWav(path, name);
            RConnection c = new RConnection();
            c.eval("setwd('" + path + "')");
            c.eval("source('sounds.R')");
            c.eval("processWav('"+wavFilename+"','"+txtFilename+"')");
          
          c.close();
                  } catch (RserveException ex) {
            Logger.getLogger(recorder.class.getName()).log(Level.SEVERE, null, ex);
        }
	}

 public static int unsignedByteToInt(byte b) {
    return (int) b & 0xFF;
    }


	private static void out(String strMessage)
	{
		System.out.println(strMessage);
	}
}



