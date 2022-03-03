import TSim.*;
import java.util.concurrent.Semaphore;




public class Lab1 {

  public Lab1(int speed1, int speed2) {
    TSimInterface tsi = TSimInterface.getInstance();

    try {
      tsi.setSpeed(1,speed1);
      tsi.setSpeed(2,speed2);
      tsi.setSwitch(17, 7, TSimInterface.SWITCH_LEFT);
     
    }
    catch (CommandException e) {
      e.printStackTrace();    // or only e.getMessage() for the error
      System.exit(1);
    }
    
    Thread train1 = new Thread(new Train(2, speed2));
    Thread train2 = new Thread(new Train(1, speed1));
    
    train1.start();
    train2.start();
  }
  
  
  class Train implements Runnable {
    
    int id;
    int speed;
  
      public Train(int id, int speed) {
          this.id = id;
          this.speed = speed;
      }
    
      @Override
      public void run() {
        try {
            TSimInterface tsi = TSimInterface.getInstance();
            SensorEvent se = tsi.getSensor(id);
            
            //Sensor and switch at the bottom of the map (near train station)
            if(se.getXpos() == 1 & se.getYpos() == 11 & se.getStatus() == SensorEvent.ACTIVE) {
                tsi.setSwitch(3, 11, TSimInterface.SWITCH_RIGHT);
            }
            else if(se.getXpos() == 2 & se.getYpos() == 9 & se.getStatus() == SensorEvent.ACTIVE) {
                tsi.setSwitch(4, 9, TSimInterface.SWITCH_RIGHT);
            }
            else if(se.getXpos() == 17 & se.getYpos() == 10 & se.getStatus() == SensorEvent.ACTIVE) {
                tsi.setSwitch(15, 9, TSimInterface.SWITCH_LEFT);
            }
            else if(se.getXpos() == 19 & se.getYpos() == 7 & se.getStatus() == SensorEvent.ACTIVE) {
                tsi.setSwitch(17, 7, TSimInterface.SWITCH_RIGHT);
            }
            else if(se.getXpos() == 15 & se.getYpos() == 7 & se.getStatus() == SensorEvent.ACTIVE) {
                tsi.setSwitch(17, 7, TSimInterface.SWITCH_RIGHT);
            }
            
        } catch (CommandException | InterruptedException e) {
          e.printStackTrace();
          System.exit(1);
        }
        
        
      }
      
    }
    

  }   
