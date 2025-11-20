library(shiny)
library(base64enc)

ui <- fluidPage(
  tags$head(
    tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/three.js/r128/three.min.js"),
    tags$script(src = "https://cdn.jsdelivr.net/npm/three@0.128.0/examples/js/loaders/GLTFLoader.js"),
    tags$script(src = "https://cdn.jsdelivr.net/npm/three@0.128.0/examples/js/controls/OrbitControls.js"),
    tags$style(HTML("
      /* Style for the right-side 'Trifold' menu */
      #electrode_data_panel {
        transition: transform 0.3s ease-in-out;
      }
      .panel-visible {
        transform: translateX(0);
      }
      .panel-hidden {
        transform: translateX(100%);
      }
    "))
  ),
  
  titlePanel("Interactive 3D Brain Model - 32 Channel System"),
  
  # --- DEBUG INFO (Top Left) ---
  tags$div(
    id = "rotation-debug",
    style = "position: fixed; top: 10px; right: 10px; background: rgba(0,0,0,0.7); color: white; padding: 10px; font-family: monospace; z-index: 1000; border-radius: 5px;",
    tags$div(id = "cam-info", "Camera Position:"),
    tags$div(id = "cam-x", "X: 0"),
    tags$div(id = "cam-y", "Y: -6.00"),
    tags$div(id = "cam-z", "Z: 0")
  ),
  
  # --- ELECTRODE DETAIL PANEL (The 'Trifold' Menu) ---
  absolutePanel(
    id = "electrode_data_panel",
    class = "panel-hidden", # Starts hidden
    top = 0, bottom = 0, right = 0, width = 400,
    style = "background-color: white; z-index: 2000; padding: 20px; border-left: 1px solid #ddd; box-shadow: -2px 0 10px rgba(0,0,0,0.1); overflow-y: auto;",
    
    # Panel Header
    div(style="display:flex; justify-content:space-between; align-items:center; margin-bottom: 20px;",
        h3(textOutput("panel_title"), style="margin:0;"),
        actionButton("close_panel", "✕", class = "btn-sm btn-default")
    ),
    
    # Dynamic Content based on Data Type
    uiOutput("data_content")
  ),
  
  sidebarLayout(
    sidebarPanel(
      h4("Controls"),
      fileInput(
        "brain_file", 
        "Upload Different Brain Model (optional):",
        accept = c(".glb", ".gltf")
      ),
      hr(),
      
      # === NEW DATA TYPE DROPDOWN ===
      selectInput(
        "data_type",
        "Data Type:",
        choices = c("Event Related Potential", "Motor Imagery"),
        selected = "Event Related Potential"
      ),
      
      hr(),
      h4("Overlays"),
      selectInput(
        "eeg_channels",
        "EEG montage:",
        choices  = c("None" = "none",
                     "32 channels (10-20)" = "32"),
        selected = "none"
      ),
      checkboxInput(
        "show_brodmann",
        "Show Brodmann overlays",
        value = FALSE
      ),
      hr(),
      p(strong("Mouse Controls:")),
      tags$ul(
        tags$li("Left click + drag: Rotate"),
        tags$li("Right click + drag: Pan"),
        tags$li("Scroll: Zoom"),
        tags$li(strong("Click Electrode: View Data"))
      ),
      hr(),
      actionButton("reset_view", "Reset View", class = "btn-primary")
    ),
    
    mainPanel(
      tags$div(
        id = "canvas-container",
        style = "width: 100%; height: 600px; background-color: #f0f0f0; border: 1px solid #ccc; position: relative;",
        tags$canvas(id = "brain-canvas"),
        tags$div(
          id = "loading-message",
          style = "position: absolute; top: 50%; left: 50%; transform: translate(-50%, -50%); display: none;",
          h4("Loading model...")
        )
      )
    )
  ),
  
  tags$script(HTML('
    var scene, camera, renderer, controls, brain, animationId;
    
    // Interaction Variables
    var raycaster = new THREE.Raycaster();
    var mouse = new THREE.Vector2();
    var isDragging = false;
    var mouseDownPos = new THREE.Vector2();
    
    var eegSpheres = [];
    var eegLabels  = [];
    var brodmannMeshes = [];
    var brainCenter = new THREE.Vector3(0, 0, 0);
    var brainBBox = null;
    var brainLoaded = false;

    // Standard 10-20 System (32 Channels)
    const electrodes_32 = [
      {label:"Fp1",x:-27,y:83,z:-3}, {label:"Fpz",x:0,y:87,z:-3}, {label:"Fp2",x:27,y:83,z:-3},
      {label:"F7",x:-71,y:51,z:-3},  {label:"F3",x:-52,y:52,z:47}, {label:"Fz",x:0,y:63,z:61}, {label:"F4",x:52,y:52,z:47}, {label:"F8",x:71,y:51,z:-3},
      {label:"FC5",x:-78,y:25,z:31}, {label:"FC1",x:-25,y:43,z:72}, {label:"FC2",x:25,y:43,z:72}, {label:"FC6",x:78,y:25,z:31},
      {label:"T7",x:-87,y:0,z:-3},   {label:"C3",x:-63,y:0,z:61},  {label:"Cz",x:0,y:0,z:88},  {label:"C4",x:63,y:0,z:61},  {label:"T8",x:87,y:0,z:-3},
      {label:"CP5",x:-78,y:-25,z:31},{label:"CP1",x:-24,y:-24,z:81},{label:"CP2",x:24,y:-24,z:81},{label:"CP6",x:78,y:-25,z:31},
      {label:"P7",x:-71,y:-51,z:-3}, {label:"P3",x:-52,y:-52,z:47}, {label:"Pz",x:0,y:-63,z:61}, {label:"P4",x:52,y:-52,z:47}, {label:"P8",x:71,y:-51,z:-3},
      {label:"POz",x:0,y:-82,z:31},  {label:"O1",x:-27,y:-83,z:-3}, {label:"Oz",x:0,y:-87,z:-3}, {label:"O2",x:27,y:-83,z:-3},
      {label:"AFz",x:0,y:82,z:31},   {label:"FCz",x:0,y:34,z:81}
    ];

    function initScene() {
      var container = document.getElementById("canvas-container");
      var canvas = document.getElementById("brain-canvas");
      
      scene = new THREE.Scene();
      scene.background = new THREE.Color(0xf0f0f0);
      
      camera = new THREE.PerspectiveCamera(75, container.clientWidth / container.clientHeight, 0.1, 1000);
      camera.position.set(0, -6, 0);
      camera.up.set(0, 0, 1);
      camera.lookAt(0, 0, 0);
      
      renderer = new THREE.WebGLRenderer({ canvas: canvas, antialias: true });
      renderer.setSize(container.clientWidth, container.clientHeight);
      renderer.setPixelRatio(window.devicePixelRatio);
      renderer.outputEncoding = THREE.sRGBEncoding;
      
      controls = new THREE.OrbitControls(camera, renderer.domElement);
      controls.enableDamping = true;
      controls.dampingFactor = 0.05;
      
      var hemiLight = new THREE.HemisphereLight(0xffffff, 0x444444, 0.7);
      hemiLight.position.set(0, 0, 5); 
      scene.add(hemiLight);
      
      var dirLight = new THREE.DirectionalLight(0xffffff, 0.8);
      dirLight.position.set(5, -5, 5);
      scene.add(dirLight);
      
      canvas.addEventListener("pointerdown", onPointerDown, false);
      canvas.addEventListener("click", onMouseClick, false);
      canvas.addEventListener("mousemove", onMouseMove, false);
      
      window.addEventListener("resize", onWindowResize, false);
      animate();
    }

    function onPointerDown(event) {
      mouseDownPos.x = event.clientX;
      mouseDownPos.y = event.clientY;
    }
    
    function onMouseMove(event) {
      if (!eegSpheres.length) return;
      var rect = renderer.domElement.getBoundingClientRect();
      mouse.x = ((event.clientX - rect.left) / rect.width) * 2 - 1;
      mouse.y = -((event.clientY - rect.top) / rect.height) * 2 + 1;
      
      raycaster.setFromCamera(mouse, camera);
      var intersects = raycaster.intersectObjects(eegSpheres);
      
      if (intersects.length > 0) {
        document.body.style.cursor = "pointer";
      } else {
        document.body.style.cursor = "default";
      }
    }

    function onMouseClick(event) {
      var dx = event.clientX - mouseDownPos.x;
      var dy = event.clientY - mouseDownPos.y;
      var dist = Math.sqrt(dx*dx + dy*dy);
      if (dist > 5) return; 
      
      var rect = renderer.domElement.getBoundingClientRect();
      mouse.x = ((event.clientX - rect.left) / rect.width) * 2 - 1;
      mouse.y = -((event.clientY - rect.top) / rect.height) * 2 + 1;
      
      raycaster.setFromCamera(mouse, camera);
      var intersects = raycaster.intersectObjects(eegSpheres);
      
      if (intersects.length > 0) {
        var obj = intersects[0].object;
        if (obj.userData && obj.userData.label) {
          var label = obj.userData.label;
          eegSpheres.forEach(s => s.material.emissive.setHex(0x000000));
          obj.material.emissive.setHex(0x555555); 
          
          if (typeof Shiny !== "undefined") {
            Shiny.setInputValue("clicked_electrode", label, {priority: "event"});
          }
        }
      }
    }

    function makeTextSprite(message) {
      var canvas = document.createElement("canvas");
      var size = 512; 
      canvas.width = size;
      canvas.height = size;
      var ctx = canvas.getContext("2d");
      
      ctx.font = "bold 200px Arial";
      ctx.textAlign = "center";
      ctx.textBaseline = "middle";
      
      ctx.strokeStyle = "rgba(0, 0, 0, 0.8)";
      ctx.lineWidth = 12;
      ctx.strokeText(message, size / 2, size / 2);
      
      ctx.fillStyle = "white";
      ctx.fillText(message, size / 2, size / 2);
      
      var texture = new THREE.CanvasTexture(canvas);
      texture.minFilter = THREE.LinearFilter;
      
      var material = new THREE.SpriteMaterial({ map: texture, transparent: true, depthTest: false, depthWrite: false });
      var sprite = new THREE.Sprite(material);
      sprite.scale.set(0.35, 0.35, 1.0); 
      return sprite;
    }

    function clearEEGElectrodes() {
      for (var i = 0; i < eegSpheres.length; i++) { scene.remove(eegSpheres[i]); }
      for (var j = 0; j < eegLabels.length; j++) { scene.remove(eegLabels[j]); }
      eegSpheres = [];
      eegLabels  = [];
    }

    function asaToSurfacePosition(asaX, asaY, asaZ) {
      if (!brainBBox) return new THREE.Vector3(0, 0, 0);
      var asaVec = new THREE.Vector3(asaX, asaY, asaZ);
      asaVec.normalize();
      
      var brainX = asaVec.y;  
      var brainY = asaVec.x;  
      var brainZ = asaVec.z;  
      
      var dir = new THREE.Vector3(brainX, brainY, brainZ);
      dir.normalize();
      
      var size = brainBBox.getSize(new THREE.Vector3());
      var surfacePos = new THREE.Vector3(
        dir.x * size.x / 2 * 1.1, 
        dir.y * size.y / 2 * 1.1, 
        dir.z * size.z / 2 * 1.1
      );
      return surfacePos.add(brainCenter);
    }

    function createEEGElectrodes(config) {
      clearEEGElectrodes();
      if (!config || config === "none") return;
      if (!brainLoaded || !brain || !brainBBox) return;
      
      var list = electrodes_32;
      var sphereSize = 0.15; 
      
      for (var i = 0; i < list.length; i++) {
        var elec = list[i];
        var pos = asaToSurfacePosition(elec.x, elec.y, elec.z);
        
        var geom = new THREE.SphereGeometry(sphereSize, 16, 16);
        var mat = new THREE.MeshStandardMaterial({ color: 0xFF0000, metalness: 0.2, roughness: 0.2 });
        var sphere = new THREE.Mesh(geom, mat);
        sphere.position.copy(pos);
        
        sphere.userData = { label: elec.label };
        
        eegSpheres.push(sphere);
        scene.add(sphere);
        
        var labelSprite = makeTextSprite(elec.label);
        labelSprite.position.copy(pos); 
        eegLabels.push(labelSprite);
        scene.add(labelSprite);
      }
    }

    function clearBrodmann() {
      for (var i = 0; i < brodmannMeshes.length; i++) { scene.remove(brodmannMeshes[i]); }
      brodmannMeshes = [];
    }

    function createBrodmannOverlays() {
      clearBrodmann();
      if (!brainLoaded || !brainBBox) return;
      var size = brainBBox.getSize(new THREE.Vector3());
      var r = Math.max(size.x, size.y, size.z) * 0.25;
      
      var defs = [
        { name: "BA17", color: 0x4b9cd3, pos: new THREE.Vector3(-size.x*0.3, 0, 0) }, 
        { name: "BA4",  color: 0xe67e22, pos: new THREE.Vector3(0, 0, size.z * 0.3) }, 
        { name: "BA10", color: 0x9b59b6, pos: new THREE.Vector3(size.x*0.35, 0, 0) } 
      ];
      defs.forEach(function(d) {
        var geom = new THREE.SphereGeometry(r, 32, 32);
        var mat = new THREE.MeshStandardMaterial({ color: d.color, transparent: true, opacity: 0.25 });
        var mesh = new THREE.Mesh(geom, mat);
        mesh.position.copy(d.pos.clone().add(brainCenter));
        brodmannMeshes.push(mesh);
        scene.add(mesh);
      });
    }

    function loadModel(fileDataUrl) {
      document.getElementById("loading-message").style.display = "block";
      if (brain) {
        scene.remove(brain);
        brain = null;
        brainLoaded = false;
        clearEEGElectrodes();
        clearBrodmann();
      }
      var loader = new THREE.GLTFLoader();
      loader.load(fileDataUrl, function(gltf) {
        brain = gltf.scene;
        brain.traverse(function(child) {
          if (child.isMesh) {
            if (child.geometry && child.geometry.computeVertexNormals) { child.geometry.computeVertexNormals(); }
            child.material = new THREE.MeshStandardMaterial({ color: 0xeeeeee, metalness: 0.1, roughness: 0.5 });
            child.material.side = THREE.DoubleSide;
          }
        });
        
        var box = new THREE.Box3().setFromObject(brain);
        var center = box.getCenter(new THREE.Vector3());
        var size = box.getSize(new THREE.Vector3());
        var maxDim = Math.max(size.x, size.y, size.z);
        var scale = 4 / maxDim;
        
        brain.scale.multiplyScalar(scale);
        brain.position.sub(center.multiplyScalar(scale));
        
        brain.updateMatrixWorld();
        brainBBox = new THREE.Box3().setFromObject(brain);
        brainLoaded = true;
        
        scene.add(brain);
        document.getElementById("loading-message").style.display = "none";
      }, undefined, function(error) {
        console.error("Error:", error);
        document.getElementById("loading-message").innerHTML = "<h4 style=color:red;>Error loading model</h4>";
      });
    }
    
    function animate() {
      animationId = requestAnimationFrame(animate);
      var camX = camera.position.x.toFixed(2);
      var camY = camera.position.y.toFixed(2);
      var camZ = camera.position.z.toFixed(2);
      document.getElementById("cam-x").textContent = "X: " + camX;
      document.getElementById("cam-y").textContent = "Y: " + camY;
      document.getElementById("cam-z").textContent = "Z: " + camZ;
      
      controls.update();
      renderer.render(scene, camera);
    }
    
    function onWindowResize() {
      var container = document.getElementById("canvas-container");
      camera.aspect = container.clientWidth / container.clientHeight;
      camera.updateProjectionMatrix();
      renderer.setSize(container.clientWidth, container.clientHeight);
    }
    
    function resetView() {
      camera.position.set(0, -6, 0);
      camera.up.set(0, 0, 1);
      camera.lookAt(0, 0, 0);
      controls.update();
    }
    
    document.addEventListener("DOMContentLoaded", function() { initScene(); });
    
    if (typeof Shiny !== "undefined") {
      Shiny.addCustomMessageHandler("loadModel", function(message) { loadModel(message); });
      Shiny.addCustomMessageHandler("resetView", function(message) { resetView(); });
      Shiny.addCustomMessageHandler("setEEG", function(message) {
        var cfg = (message && message.n) ? message.n : "none";
        createEEGElectrodes(cfg);
      });
      Shiny.addCustomMessageHandler("setBrodmannVisible", function(message) {
        var show = !!(message && message.show);
        if (show) { createBrodmannOverlays(); } else { clearBrodmann(); }
      });
    }
  '))
)

server <- function(input, output, session) {
  
  # Reactive value to store the selected electrode
  selected_electrode <- reactiveVal(NULL)
  
  observeEvent(TRUE, {
    desktop_dir <- "~/Desktop"
    desktop_dir <- normalizePath(desktop_dir, winslash = "/", mustWork = FALSE)
    glb_files <- list.files(desktop_dir, pattern = "\\.glb$", full.names = TRUE)
    
    if (length(glb_files) == 0) {
      showNotification(paste("No .glb files found on Desktop at:", desktop_dir), type = "error")
      return()
    }
    preferred <- grepl("Brain_MRI", basename(glb_files), ignore.case = TRUE)
    if (any(preferred)) {
      default_brain_path <- glb_files[which(preferred)[1]]
    } else {
      default_brain_path <- glb_files[1]
    }
    message("Using default brain model: ", default_brain_path)
    file_data   <- readBin(default_brain_path, "raw", file.info(default_brain_path)$size)
    file_base64 <- paste0("data:model/gltf-binary;base64,", base64encode(file_data))
    session$sendCustomMessage("loadModel", file_base64)
  }, once = TRUE)
  
  observeEvent(input$brain_file, {
    req(input$brain_file)
    tryCatch({
      file_path   <- input$brain_file$datapath
      file_data   <- readBin(file_path, "raw", file.info(file_path)$size)
      file_base64 <- paste0("data:model/gltf-binary;base64,", base64encode(file_data))
      session$sendCustomMessage("loadModel", file_base64)
    }, error = function(e) {
      showNotification(paste("Error loading file:", e$message), type = "error")
    })
  })
  
  # === OBSERVE CLICK FROM JS ===
  observeEvent(input$clicked_electrode, {
    selected_electrode(input$clicked_electrode)
    runjs(sprintf('
      document.getElementById("electrode_data_panel").classList.remove("panel-hidden");
      document.getElementById("electrode_data_panel").classList.add("panel-visible");
    '))
  })
  
  # === OBSERVE CLOSE BUTTON ===
  observeEvent(input$close_panel, {
    selected_electrode(NULL)
    runjs(sprintf('
      document.getElementById("electrode_data_panel").classList.remove("panel-visible");
      document.getElementById("electrode_data_panel").classList.add("panel-hidden");
    '))
  })
  
  # === OUTPUTS FOR THE MENU ===
  output$panel_title <- renderText({
    req(selected_electrode())
    paste("Electrode:", selected_electrode())
  })
  
  # Dynamic Content Switcher
  output$data_content <- renderUI({
    req(selected_electrode())
    
    if (input$data_type == "Event Related Potential") {
      tagList(
        hr(),
        h4("Signal Analysis"),
        plotOutput("placeholder_plot", height = "250px"),
        p(em("Figure 1: Real-time signal amplitude over 500ms epoch.")),
        hr(),
        h4("Statistics"),
        tableOutput("placeholder_table"),
        p(style="color:#666;", "This table structure is ready for your custom data.")
      )
    } else {
      # Motor Imagery - Blank for now
      tagList(
        hr(),
        h4("Motor Imagery Data"),
        p("No data available for this modality yet.")
      )
    }
  })
  
  output$placeholder_plot <- renderPlot({
    req(selected_electrode())
    # DUMMY PLOT: Generate random signal
    t <- seq(0, 1, length.out=100)
    y <- sin(2*pi*10*t) + rnorm(100, 0, 0.2)
    plot(t, y, type="l", col="blue", lwd=2, 
         main=paste("Raw Signal:", selected_electrode()), 
         xlab="Time (s)", ylab="Amplitude (uV)")
    grid()
  })
  
  output$placeholder_table <- renderTable({
    req(selected_electrode())
    # DUMMY TABLE
    data.frame(
      Metric = c("Mean Amplitude", "Peak Frequency", "Alpha Power", "Beta Power"),
      Value  = c(round(runif(1, -5, 5), 2), "10.5 Hz", "12.3 uV^2", "4.5 uV^2")
    )
  })
  
  observe({
    session$sendCustomMessage("setEEG", list(n = input$eeg_channels))
  })
  
  observe({
    session$sendCustomMessage("setBrodmannVisible", list(show = isTRUE(input$show_brodmann)))
  })
  
  observeEvent(input$reset_view, {
    session$sendCustomMessage("resetView", TRUE)
  })
}

runjs <- function(code) {
  session <- shiny::getDefaultReactiveDomain()
  session$sendCustomMessage("shiny-run-js", code)
}

ui_final <- tagList(ui, tags$script(HTML('
  Shiny.addCustomMessageHandler("shiny-run-js", function(code) {
    eval(code);
  });
')))

shinyApp(ui = ui_final, server = server)
