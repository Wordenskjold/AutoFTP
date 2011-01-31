/*
	Terminal program that monitors the current directory and automatically uploads 
	changed or new files to an ftp-server specified. 
	
	Hidden files are ignored, and the folder is not synchronized with the server on startup.
	New directory structures are created as needed, so pasting a folder containing files to 
	the monitored folder or sub-folders, will upload the structure and content to the server.
	
	The server connection is checked on startup, but the program does not maintain the connection by default.
	
	If the program is started by autoftp [server] [username] [password], the local folder will be the current working directory.
	If the user does not include any startup parameters (like if the program is started through sbt with sbt run), 
	the wizard will be started, and the user will have to enter the lcoal folder to monitor.
*/

import java.io.File
import com.enterprisedt.net.ftp._
import scala.actors.Actor
import scala.actors.Actor._
import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer

//TODO: Flags: ms between each check, lazy uploading(only connect when new file is found), remote root dir, ignore hidden files
//				Synchronize on startup
object AutoFTP{
	
	def main(args : Array[String]){ 
		val usage = "Usage: autoftp [server] [username] [password]\n or just autoftp to start wizard"
		args.length match {
			case 0 => startWizard
			case 3 => AutoFTP.start( args(0), args(1), args(2) )
			case _ => println(usage)
		}
	}
	
	def startWizard = {
		val options = Array("local directory: ","server: ", "usr: ", "pwd: ")
		val input = new Array[String](4)
		for(i <- 0 to options.length-1){
			print(options(i))
			input(i) = Console.readLine
		}
		AutoFTP.rootDir = new File(input(0))
		AutoFTP.start( input(1), input(2), input(3) )
	}
	
	// The root directory to watch
	var rootDir = new File(System getProperty "user.dir")
	
	def root = { rootDir.getAbsolutePath }
	
	// Monitor object
	val monitor = new FolderMonitor(rootDir)
	
	// Server Info
	var server = ""
	var usr = ""
	var pwd = ""
	
	// Server Object
	lazy val ftp : FileTransferClient = 
	if(server != "" && usr != "" && pwd != "") new FileTransferClient()
	else null
	
	private var interrupted = false
	
	def interrupt = {
		interrupted = true
		println("Program stopped")
	}
	
	def start(server : String, usr : String, pwd : String) = {
		this.server = server
		this.usr = usr
		this.pwd = pwd
		ftp setRemoteHost server
		ftp setUserName usr
		ftp setPassword pwd
		if(testServer) watch
	}
	
	def watch = {
		println("Watching root %s for changes... \n[enter] exits" format rootDir)
		// Start the interrupter so user can stop program
		interrupter.start
		// Init and start the monitor, which recursively monitors subfolders
		monitor.init 
	}
	
	def testServer = {
		def success = println("Connection OK")
		def failure = throw new Exception("Could not connect to server. Program exits...")
		ftp.connect
		if(ftp.isConnected) { success; ftp.disconnect }
		else failure
		true
	}
	
	def connect = {
		if(!ftp.isConnected) ftp.connect
		ftp.isConnected
	}
	
	def ensurePath(path : String) = {
		println("The path: " + path)
		val split = path.split("/")
		var tmpPath = ""
		for(i <- 0 until split.length-1){
			tmpPath += split(i)+"/"
			println("split: " + tmpPath)
			if(!ftp.exists(tmpPath.substring(0,tmpPath.length-2))){
				try{ ftp.createDirectory(tmpPath) }
				catch{ case e : Exception => /* ... */ }
			}
		}
	}
	
	def upload(file : File, relPath : String) = {
		if(connect){
			// Make sure the relPath on the server is accessible, else create the folders needed (BUGGY)
			println("Uploading file %s to path %s".format(file, relPath))
			if(!ftp.exists(relPath.substring(0,relPath.length-2))) ensurePath(relPath)
			// Upload file
			ftp.uploadFile(file.getPath, relPath)
			println("File %s uploaded successfully".format(file))
		}
		else{
			println("Could not upload file file %s\n: Server refused connection".format(file))
		}	
	}
	
	/* interrupts the AutoFTP object when enter is pressed in the concole */
	object interrupter extends Actor{
		override def act = {
			for( ln <- scala.io.Source.stdin.getLines ) {
				if(ln == ""){ AutoFTP.interrupt; exit() }
			}
		}
	}
	
	/* Default properties */
	object props {
		// How often to check the tree for changes (ms)
		var interval = 1000;
	}
	
	class FolderMonitor(root : File) extends Actor{
		
		var lastModified : Long = root.lastModified
		val filesModified = new HashMap[File,Long]
		val dirtyBuffer = ArrayBuffer[File]()
	
		def sleep = Thread sleep props.interval
	
		def init{
			root.listFiles.foreach{	
				file => 
				if(file.isDirectory) new FolderMonitor(file).init
				filesModified += file -> file.lastModified
			}
			start
		}
		
		def manageFile(file : File) = {
			if(file.isDirectory) new FolderMonitor(file).init
			else dirtyBuffer append file
			filesModified += file -> file.lastModified
		}
		
		def fetchDirtyFiles = {
			root.listFiles.foreach{
				file =>
				if(!(filesModified contains file) && !file.isHidden) manageFile(file)
				else if(!file.isDirectory && !file.isHidden && file.lastModified > filesModified(file)){
					filesModified(file) = file.lastModified
					dirtyBuffer append file
				}
			}
		}
		
		def uploadFiles = {
			var relPath = ""
			dirtyBuffer.foreach{ 
				file =>
				// Find the path relative to the server root
				relPath = file.getPath.split(AutoFTP.root)(1)
				// Remove the first /
				relPath = relPath.substring(1,relPath.length)
				// Upload file (exiting files will be overwritten)
				AutoFTP.upload(file, relPath)
				// Disconnect from the server (should be optional)
				AutoFTP.ftp.disconnect
			}
			dirtyBuffer.clear
		}
		
		override def act{
			while(root.exists && !AutoFTP.interrupted){
				if(root.lastModified > lastModified) {
					lastModified = root.lastModified
					fetchDirtyFiles
					if(dirtyBuffer.length != 0) uploadFiles
				}
				sleep
			}
		}
	}
}