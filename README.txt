This project depends on the Docker container "gedisimulator_r", located within the "GEDI_CalVal" folder. The steps below describe how to set up the environment and begin working with the project.

1. Place the repository
   - Move the "GEDI_CalVal" folder into your Documents directory.
	Inside "GEDI_CalVal/gedisimulator_r" you will find the Dockerfile, which is required to build the container.

2. Navigate to the Dockerfile
   - Open a command prompt and move into the "GEDI_CalVal/gedisimulator_r" directory. For example:
	cd ~/Documents/GEDI_CalVal/gedisimulator_r

3. Build the container
   - Run the following to build and start the container: 
	docker compose up --build -d
   - Note: this step may take some time on the first run.

4. Shut down the container
   - After the build completes, stop and remove the running container with:
	docker compose down

   - You will also use this command whenever you finish working to free up system resources used by Docker.

5. Start an interactive session
   - Link the Docker container to your GEDI_CalVal folder and enter the container:
	docker run -it --rm -v "C:\Users\owner\Documents\GEDI_CalVal:/workspace/GEDI_CalVal" gedisimulator:v1.0 bash"

   Inside the container:

	"/root/renv" contains the R environment. Run "R" from here to start R.

	"/root/src" contains the GEDI simulator source code.

	"/workspace/GEDI_CalVal" is your project folder (linked from the host). To navigate to it:
		cd /workspace/GEDI_CalVal

6. Run the Cal/Val workflow
   - To convert your plot measurements into GEDI-aligned format, follow the instructions in "/Tutorial/CalVal_Workflow.html".
   - It is recommended to run this workflow outside Docker in your normal R environment.
   - Once finalized, re-enter Docker, navigate to "/root/renv", open R, and continue with Step 10 from the workflow instructions.

7. Run the waveform simulator
   - To generate simulated GEDI waveforms from plot and ALS data:

	Navigate to /root/renv inside the container.

	Open R by typing: R

	Follow the instructions in "GEDI Waveform Simulator Workflow", found in the GEDI_CalVal/Tutorial folder.

Notes:
- Always use "docker compose down" when you are done working to clean up resources.
- Using "--rm" ensures containers are removed when they exit, so you do not leave stopped containers behind.
 