library(shiny)
library(base64enc)
library(ggplot2)
library(ggiraph)
library(dplyr)
library(readr)
library(gt) 

ui <- fluidPage(
  tags$head(
    tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/three.js/r128/three.min.js"),
    tags$script(src = "https://cdn.jsdelivr.net/npm/three@0.128.0/examples/js/loaders/GLTFLoader.js"),
    tags$script(src = "https://cdn.jsdelivr.net/npm/three@0.128.0/examples/js/controls/OrbitControls.js"),
    tags$style(HTML("
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
  
  # --- P-VALUE LEGEND ---
  tags$div(
    style = "position: fixed; bottom: 20px; right: 20px; background: white; padding: 15px; border-radius: 8px; border: 1px solid #ccc; box-shadow: 0 0 10px rgba(0,0,0,0.1); z-index: 1000;",
    h5("Significance (P-Value)", style="margin: 0 0 10px 0; font-weight: bold;"),
    # Not Significant
    div(style="display: flex; align-items: center; margin-bottom: 5px;",
        span(style="width: 20px; height: 20px; background: #ffffff; border: 1px solid #ccc; display: inline-block; margin-right: 8px;"),
        span("> 0.05 (Not Sig)")
    ),
    # Significant (Light Red)
    div(style="display: flex; align-items: center; margin-bottom: 5px;",
        span(style="width: 20px; height: 20px; background: #ffcccc; border: 1px solid #ccc; display: inline-block; margin-right: 8px;"),
        span("< 0.05 (Sig)")
    ),
    # Highly Significant (Deep Red Gradient)
    div(style="display: flex; align-items: center;",
        # Gradient from light to deep red to show intensity
        span(style="width: 20px; height: 20px; background: linear-gradient(to bottom, #ff9999, #ff0000); border: 1px solid #ccc; display: inline-block; margin-right: 8px;"),
        span("< 0.001 (Highly Sig)")
    )
  ),
  
  # --- ELECTRODE DETAIL PANEL ---
  absolutePanel(
    id = "electrode_data_panel",
    class = "panel-hidden", 
    top = 0, bottom = 0, right = 0, width = 600, 
    style = "background-color: white; z-index: 2000; padding: 20px; border-left: 1px solid #ddd; box-shadow: -2px 0 10px rgba(0,0,0,0.1); overflow-y: auto;",
    
    div(style="display:flex; justify-content:space-between; align-items:center; margin-bottom: 20px;",
        h3(textOutput("panel_title"), style="margin:0;"),
        actionButton("close_panel", "✕", class = "btn-sm btn-default")
    ),
    uiOutput("data_content")
  ),
  
  sidebarLayout(
    sidebarPanel(
      h4("Controls"),
      fileInput("brain_file", "Upload Different Brain Model (optional):", accept = c(".glb", ".gltf")),
      hr(),
      
      # [INSTRUCTION] HIDDEN CONTROLS
      conditionalPanel(
        condition = "false", 
        wellPanel(
          style = "background: #f9f9f9;",
          h5(strong("Condition")),
          selectInput("mi_task", NULL, choices = c("Real Movement" = "real", "Imagined Movement" = "imagined", "Difference" = "diff"))
        )
      ),
      
      hr(),
      p(strong("Mouse Controls:")),
      tags$ul(tags$li("Left click + drag: Rotate"), tags$li("Right click + drag: Pan"), tags$li("Scroll: Zoom"), tags$li(strong("Click Electrode: View Data"))),
      hr(),
      actionButton("reset_view", "Reset View", class = "btn-primary")
    ),
    
    mainPanel(
      tags$div(
        id = "canvas-container",
        style = "width: 100%; height: 600px; background-color: #f0f0f0; border: 1px solid #ccc; position: relative;",
        tags$canvas(id = "brain-canvas"),
        tags$div(id = "loading-message", style = "position: absolute; top: 50%; left: 50%; transform: translate(-50%, -50%); display: none;", h4("Loading model..."))
      )
    )
  ),
  
  tags$script(HTML('
    var scene, camera, renderer, controls, brain, animationId;
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
    
    var currentDataType = "ERP"; 
    var currentTask = "real";
    var electrodePValues = {}; 

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
      if (intersects.length > 0) { document.body.style.cursor = "pointer"; } else { document.body.style.cursor = "default"; }
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
          obj.material.emissive.setHex(0xffffff); 
          setTimeout(function() { obj.material.emissive.setHex(0x000000); }, 200);
          if (typeof Shiny !== "undefined") { Shiny.setInputValue("clicked_electrode", label, {priority: "event"}); }
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
      var brainX = asaVec.y; var brainY = asaVec.x; var brainZ = asaVec.z;  
      var dir = new THREE.Vector3(brainX, brainY, brainZ);
      dir.normalize();
      var size = brainBBox.getSize(new THREE.Vector3());
      var surfacePos = new THREE.Vector3(dir.x * size.x / 2 * 1.1, dir.y * size.y / 2 * 1.1, dir.z * size.z / 2 * 1.1);
      return surfacePos.add(brainCenter);
    }
    
    function updateVisuals() {
      if (eegSpheres.length === 0) return;
      
      var activeSet = [];
      if (currentDataType === "MI") {
        if (currentTask === "real")     activeSet = ["C3", "CP3", "FC3"]; 
        if (currentTask === "imagined") activeSet = ["C3", "FCz", "Cz"];
        if (currentTask === "diff")     activeSet = ["FCz", "Cz"];
      }
      
      eegSpheres.forEach(function(sphere) {
        var label = sphere.userData.label;
        sphere.scale.set(1, 1, 1); 
        sphere.material.transparent = false; 
        
        if (currentDataType === "ERP") {
          var pval = electrodePValues[label];
          if (pval !== undefined && pval <= 0.05) {
            // Map p=0.05 -> Light Red, p=0.001 -> Deep Red
            var intensity = pval / 0.05; 
            if (intensity < 0) intensity = 0;
            sphere.material.color.setRGB(1.0, intensity, intensity);
            sphere.material.opacity = 1.0;
          } else {
            sphere.material.color.setHex(0xffffff); 
            sphere.material.opacity = 1.0;
          }
        } else if (currentDataType === "MI") {
          if (activeSet.includes(label)) {
             sphere.material.color.setHex(0xFF4500); 
             sphere.material.opacity = 1.0;
             sphere.material.emissive.setHex(0x330000); 
          } else {
             sphere.material.color.setHex(0x888888); 
             sphere.material.opacity = 0.5;
             sphere.material.transparent = true; 
             sphere.material.emissive.setHex(0x000000);
          }
        }
      });
    }

    function createEEGElectrodes(config) {
      clearEEGElectrodes();
      if (!brainLoaded || !brain || !brainBBox) return;
      var list = electrodes_32;
      var sphereSize = 0.15; 
      for (var i = 0; i < list.length; i++) {
        var elec = list[i];
        var pos = asaToSurfacePosition(elec.x, elec.y, elec.z);
        var geom = new THREE.SphereGeometry(sphereSize, 32, 32);
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
      updateVisuals();
    }
    
    function clearBrodmann() { for (var i = 0; i < brodmannMeshes.length; i++) { scene.remove(brodmannMeshes[i]); } brodmannMeshes = []; }
    function createBrodmannOverlays() {
      clearBrodmann();
      if (!brainLoaded || !brainBBox) return;
      var size = brainBBox.getSize(new THREE.Vector3());
      var r = Math.max(size.x, size.y, size.z) * 0.25;
      var defs = [{name:"BA17",color:0x4b9cd3,pos:new THREE.Vector3(-size.x*0.3,0,0)},{name:"BA4",color:0xe67e22,pos:new THREE.Vector3(0,0,size.z*0.3)},{name:"BA10",color:0x9b59b6,pos:new THREE.Vector3(size.x*0.35,0,0)}];
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
      if (brain) { scene.remove(brain); brain = null; brainLoaded = false; clearEEGElectrodes(); clearBrodmann(); }
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
        brain.rotation.z = Math.PI;
        brain.updateMatrixWorld();
        brainBBox = new THREE.Box3().setFromObject(brain);
        brainLoaded = true;
        scene.add(brain);
        document.getElementById("loading-message").style.display = "none";
        
        // Trigger creation of default 32 electrodes
        createEEGElectrodes("32");
        
      }, undefined, function(error) { console.error("Error:", error); });
    }
    function animate() {
      animationId = requestAnimationFrame(animate);
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
      Shiny.addCustomMessageHandler("setEEG", function(message) { var cfg = (message && message.n) ? message.n : "none"; createEEGElectrodes(cfg); });
      Shiny.addCustomMessageHandler("setBrodmannVisible", function(message) { var show = !!(message && message.show); if (show) { createBrodmannOverlays(); } else { clearBrodmann(); } });
      Shiny.addCustomMessageHandler("setDataType", function(message) { currentDataType = message.type; updateVisuals(); });
      Shiny.addCustomMessageHandler("setMITask", function(message) { currentTask = message.task; updateVisuals(); });
      Shiny.addCustomMessageHandler("updateElectrodeColors", function(message) { electrodePValues = message; updateVisuals(); });
    }
  '))
)

server <- function(input, output, session) {
  
  selected_electrode <- reactiveVal(NULL)
  
  simulated_erp_data <- reactive({
    set.seed(42) 
    times <- seq(-200, 800, by = 10)
    channels_list <- c("Fp1", "Fpz", "Fp2", "F7", "F3", "Fz", "F4", "F8", 
                       "FC5", "FC1", "FC2", "FC6", "T7", "C3", "Cz", "C4", 
                       "T8", "CP5", "CP1", "CP2", "CP6", "P7", "P3", "Pz", 
                       "P4", "P8", "POz", "O1", "Oz", "O2", "AFz", "FCz")
    conditions <- c("Motion", "Nonmotion")
    
    df_list <- list()
    count <- 1
    
    for (ch in channels_list) {
      for (cond in conditions) {
        noise <- rnorm(length(times), 0, 0.5)
        signal <- 5 * exp(-((times - 350)^2) / (2 * 100^2)) 
        signal <- signal - 2 * exp(-((times - 200)^2) / (2 * 40^2))
        
        effect_size <- runif(1, 0.4, 1.0) 
        if (cond == "Nonmotion") signal <- signal * effect_size
        
        df_list[[count]] <- data.frame(
          Time_ms = times,
          Channel = ch,
          Condition = cond,
          GrandVoltage = signal + noise
        )
        count <- count + 1
      }
    }
    do.call(rbind, df_list)
  })
  
  # Calculate P-Values for coloring
  observe({
    df_all <- simulated_erp_data()
    channels <- unique(df_all$Channel)
    p_vals_list <- list()
    for (ch in channels) {
      elec_data <- df_all %>% filter(Channel == ch)
      p300_data <- elec_data %>% filter(Time_ms >= 250 & Time_ms <= 500)
      res <- tryCatch({ t.test(GrandVoltage ~ Condition, data = p300_data)$p.value }, error = function(e) 1.0)
      p_vals_list[[ch]] <- res
    }
    session$sendCustomMessage("updateElectrodeColors", p_vals_list)
  })
  
  observeEvent(TRUE, {
    desktop_dir <- "~/Desktop"
    desktop_dir <- normalizePath(desktop_dir, winslash = "/", mustWork = FALSE)
    glb_files <- list.files(desktop_dir, pattern = "\\.glb$", full.names = TRUE)
    if (length(glb_files) == 0) { showNotification(paste("No .glb files found on Desktop at:", desktop_dir), type = "error"); return() }
    preferred <- grepl("Brain_MRI", basename(glb_files), ignore.case = TRUE)
    if (any(preferred)) { default_brain_path <- glb_files[which(preferred)[1]] } else { default_brain_path <- glb_files[1] }
    message("Using default brain model: ", default_brain_path)
    file_data   <- readBin(default_brain_path, "raw", file.info(default_brain_path)$size)
    file_base64 <- paste0("data:model/gltf-binary;base64,", base64encode(file_data))
    
    session$sendCustomMessage("loadModel", file_base64)
    session$sendCustomMessage("setBrodmannVisible", list(show = FALSE))
    session$sendCustomMessage("setDataType", list(type = "ERP"))
    
  }, once = TRUE)
  
  observeEvent(input$brain_file, {
    req(input$brain_file)
    tryCatch({
      file_path   <- input$brain_file$datapath
      file_data   <- readBin(file_path, "raw", file.info(file_path)$size)
      file_base64 <- paste0("data:model/gltf-binary;base64,", base64encode(file_data))
      session$sendCustomMessage("loadModel", file_base64)
    }, error = function(e) { showNotification(paste("Error loading file:", e$message), type = "error") })
  })
  
  observeEvent(input$clicked_electrode, {
    selected_electrode(input$clicked_electrode)
    runjs(sprintf('document.getElementById("electrode_data_panel").classList.remove("panel-hidden"); document.getElementById("electrode_data_panel").classList.add("panel-visible");'))
  })
  
  observeEvent(input$close_panel, {
    selected_electrode(NULL)
    runjs(sprintf('document.getElementById("electrode_data_panel").classList.remove("panel-visible"); document.getElementById("electrode_data_panel").classList.add("panel-hidden");'))
  })
  
  output$panel_title <- renderText({ req(selected_electrode()); paste("Electrode:", selected_electrode()) })
  
  output$data_content <- renderUI({
    req(selected_electrode())
    tagList(
      hr(), 
      h4("ERP Waveform"), 
      girafeOutput("plot_erp", height = "400px"), 
      p(em("Interact: Hover points to see values, double click to reset zoom.")), 
      hr(), 
      gt_output("table_erp")
    )
  })
  
  output$plot_erp <- renderGirafe({
    req(selected_electrode())
    df_all <- simulated_erp_data()
    channel_name <- selected_electrode()
    df_channel <- df_all %>% filter(Channel == channel_name)
    req(nrow(df_channel) > 0) 
    y_max <- max(abs(df_channel$GrandVoltage), na.rm = TRUE)
    y_limits <- c(-y_max, y_max)
    
    p <- ggplot(df_channel, aes(x = Time_ms, y = GrandVoltage, color = Condition, group = Condition)) +
      geom_hline(yintercept = 0, linetype = "solid", color = "gray50", linewidth = 0.5, alpha = 0.5) +
      geom_vline(xintercept = 0, linetype = "solid", color = "black", linewidth = 1) +
      annotate("text", x = 0, y = Inf, label = "Event", vjust = -0.5, hjust = 0.5, size = 4, fontface = "bold", color = "black") +
      geom_line_interactive(
        aes(
          tooltip = paste0("Condition: ", Condition),
          data_id = Condition
        ),
        linewidth = 1.5
      ) +
      geom_point_interactive(
        aes(
          tooltip = paste0(
            "Channel: ", channel_name, "\n",
            "Condition: ", Condition, "\n",
            "Time (ms): ", Time_ms, "\n",
            "Voltage: ", sprintf("%.2f", GrandVoltage)
          ),
          data_id = paste(Condition, Time_ms)
        ),
        size = 2,
        alpha = 0.7
      ) +
      labs(
        title = paste("ERP Waveform - Channel:", channel_name),
        subtitle = "Hover over points to see values | Drag to zoom | Double-click to reset",
        x = "Time relative to event (ms)",
        y = expression("Voltage (µV)")
      ) +
      scale_x_continuous(breaks = seq(-200, 800, 100)) +
      scale_y_continuous(limits = y_limits) +
      scale_color_manual(
        values = c("Motion" = "#0072B2", "Nonmotion" = "#D55E00"),
        labels = c("Motion", "Nonmotion")
      ) +
      theme_classic(base_size = 14) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, color = "gray40"),
        legend.title = element_blank(),
        legend.position = "top",
        axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.text = element_text(color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      )
    
    girafe(
      ggobj = p,
      width_svg = 6, 
      height_svg = 4,
      options = list(
        opts_hover(css = "stroke-width:4;stroke:#FF5733;"),
        opts_tooltip(zindex = 9999),
        opts_zoom(min = 0.5, max = 4),
        opts_sizing(rescale = TRUE, width = 1),
        opts_toolbar(saveaspng = TRUE)
      )
    )
  })
  
  output$table_erp <- render_gt({
    req(selected_electrode())
    df_all <- simulated_erp_data()
    elec_data <- df_all %>% filter(Channel == selected_electrode())
    
    n200_data <- elec_data %>% filter(Time_ms >= 150 & Time_ms <= 250)
    t_n200 <- t.test(GrandVoltage ~ Condition, data = n200_data)
    n200_means <- n200_data %>% group_by(Condition) %>% summarise(m = mean(GrandVoltage))
    
    p300_data <- elec_data %>% filter(Time_ms >= 250 & Time_ms <= 500)
    t_p300 <- t.test(GrandVoltage ~ Condition, data = p300_data)
    p300_means <- p300_data %>% group_by(Condition) %>% summarise(m = mean(GrandVoltage))
    
    stats_df <- data.frame(
      Component = c("N200 (150-250ms)", "P300 (250-500ms)"),
      `Motion Mean` = c(n200_means$m[n200_means$Condition == "Motion"], p300_means$m[p300_means$Condition == "Motion"]),
      `Nonmotion Mean` = c(n200_means$m[n200_means$Condition == "Nonmotion"], p300_means$m[p300_means$Condition == "Nonmotion"]),
      t_statistic = c(t_n200$statistic, t_p300$statistic),
      p_value_raw = c(t_n200$p.value, t_p300$p.value)
    )
    
    stats_df$p_value_str <- ifelse(stats_df$p_value_raw < 0.001, "< 0.001", sprintf("%.3f", stats_df$p_value_raw))
    
    motion_col <- "#0072B2"
    nonmotion_col <- "#D55E00"
    
    stats_df %>%
      gt() %>%
      tab_header(
        title = md(paste0("**Channel ", selected_electrode(), " Statistics**")),
        subtitle = "Comparison of Mean Amplitudes (Welch's t-test)"
      ) %>%
      fmt_number(
        columns = c(`Motion.Mean`, `Nonmotion.Mean`, t_statistic),
        decimals = 3
      ) %>%
      tab_style(
        style = cell_text(color = motion_col, weight = "bold"),
        locations = cells_body(columns = `Motion.Mean`)
      ) %>%
      tab_style(
        style = cell_text(color = nonmotion_col, weight = "bold"),
        locations = cells_body(columns = `Nonmotion.Mean`)
      ) %>%
      tab_style(
        style = list(cell_text(color = "red", weight = "bold")),
        locations = cells_body(columns = p_value_str, rows = p_value_raw < 0.05)
      ) %>%
      cols_label(
        `Motion.Mean` = "Motion (µV)",
        `Nonmotion.Mean` = "Nonmotion (µV)",
        t_statistic = "t-stat",
        p_value_str = "p-value"
      ) %>%
      cols_hide(columns = p_value_raw) %>% 
      cols_align(align = "center", columns = everything()) %>%
      tab_options(table.width = pct(100))
  })
  
  observe({ session$sendCustomMessage("setBrodmannVisible", list(show = isTRUE(input$show_brodmann))) })
  observeEvent(input$reset_view, { session$sendCustomMessage("resetView", TRUE) })
}

runjs <- function(code) { session <- shiny::getDefaultReactiveDomain(); session$sendCustomMessage("shiny-run-js", code); }
ui_final <- tagList(ui, tags$script(HTML('Shiny.addCustomMessageHandler("shiny-run-js", function(code) { eval(code); });')))
shinyApp(ui = ui_final, server = server)
