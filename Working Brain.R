# RUNNING COMMENTS: 

# FIXES:
# need to have the model in here by default.
# Defualt Zoom should be closer
# turn off auto rotate



library(shiny)
library(base64enc)

ui <- fluidPage(
  tags$head(
    tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/three.js/r128/three.min.js"),
    tags$script(src = "https://cdn.jsdelivr.net/npm/three@0.128.0/examples/js/loaders/GLTFLoader.js"),
    tags$script(src = "https://cdn.jsdelivr.net/npm/three@0.128.0/examples/js/controls/OrbitControls.js")
  ),
  
  titlePanel("Interactive 3D Brain Model"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Controls"),
      fileInput("brain_file", 
                "Upload Brain Model (.glb file):",
                accept = c(".glb", ".gltf")),
      hr(),
      checkboxInput("auto_rotate", 
                    "Auto Rotate", 
                    value = TRUE),
      sliderInput("rotation_speed", 
                  "Rotation Speed:", 
                  min = 0, 
                  max = 5, 
                  value = 1, 
                  step = 0.1),
      hr(),
      p(strong("Mouse Controls:")),
      tags$ul(
        tags$li("Left click + drag: Rotate"),
        tags$li("Right click + drag: Pan"),
        tags$li("Scroll: Zoom")
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
    var autoRotate = true;
    var rotationSpeed = 0.01;
    
    function initScene() {
      var container = document.getElementById("canvas-container");
      var canvas = document.getElementById("brain-canvas");
      
      scene = new THREE.Scene();
      scene.background = new THREE.Color(0xf0f0f0);
      
      camera = new THREE.PerspectiveCamera(75, container.clientWidth / container.clientHeight, 0.1, 1000);
      camera.position.z = 5;
      
      renderer = new THREE.WebGLRenderer({ canvas: canvas, antialias: true });
      renderer.setSize(container.clientWidth, container.clientHeight);
      renderer.setPixelRatio(window.devicePixelRatio);
      
      controls = new THREE.OrbitControls(camera, renderer.domElement);
      controls.enableDamping = true;
      controls.dampingFactor = 0.05;
      controls.screenSpacePanning = false;
      controls.minDistance = 2;
      controls.maxDistance = 10;
      
      var ambientLight = new THREE.AmbientLight(0xffffff, 0.6);
      scene.add(ambientLight);
      
      var directionalLight1 = new THREE.DirectionalLight(0xffffff, 0.8);
      directionalLight1.position.set(5, 5, 5);
      scene.add(directionalLight1);
      
      var directionalLight2 = new THREE.DirectionalLight(0xffffff, 0.4);
      directionalLight2.position.set(-5, -5, -5);
      scene.add(directionalLight2);
      
      window.addEventListener("resize", onWindowResize, false);
      
      animate();
    }
    
    function loadModel(fileDataUrl) {
      document.getElementById("loading-message").style.display = "block";
      
      if (brain) {
        scene.remove(brain);
      }
      
      var loader = new THREE.GLTFLoader();
      
      loader.load(fileDataUrl, function(gltf) {
        brain = gltf.scene;
        
        var box = new THREE.Box3().setFromObject(brain);
        var center = box.getCenter(new THREE.Vector3());
        var size = box.getSize(new THREE.Vector3());
        
        var maxDim = Math.max(size.x, size.y, size.z);
        var scale = 4 / maxDim;
        brain.scale.multiplyScalar(scale);
        
        brain.position.sub(center.multiplyScalar(scale));
        
        scene.add(brain);
        document.getElementById("loading-message").style.display = "none";
      }, undefined, function(error) {
        console.error("Error loading model:", error);
        document.getElementById("loading-message").innerHTML = "<h4 style=color:red;>Error loading model</h4>";
      });
    }
    
    function animate() {
      animationId = requestAnimationFrame(animate);
      
      if (autoRotate && brain) {
        brain.rotation.y += rotationSpeed;
      }
      
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
      camera.position.set(0, 0, 5);
      controls.reset();
    }
    
    document.addEventListener("DOMContentLoaded", function() {
      initScene();
    });
    
    if (typeof Shiny !== "undefined") {
      Shiny.addCustomMessageHandler("loadModel", function(message) {
        loadModel(message);
      });
      
      Shiny.addCustomMessageHandler("updateRotation", function(message) {
        autoRotate = message.auto;
        rotationSpeed = message.speed * 0.01;
      });
      
      Shiny.addCustomMessageHandler("resetView", function(message) {
        resetView();
      });
    }
  '))
)

server <- function(input, output, session) {
  
  observeEvent(input$brain_file, {
    req(input$brain_file)
    
    tryCatch({
      file_path <- input$brain_file$datapath
      file_data <- readBin(file_path, "raw", file.info(file_path)$size)
      file_base64 <- paste0("data:model/gltf-binary;base64,", base64encode(file_data))
      
      session$sendCustomMessage("loadModel", file_base64)
    }, error = function(e) {
      showNotification(paste("Error loading file:", e$message), type = "error")
    })
  })
  
  observe({
    session$sendCustomMessage("updateRotation", 
                              list(
                                auto = input$auto_rotate,
                                speed = input$rotation_speed
                              )
    )
  })
  
  observeEvent(input$reset_view, {
    session$sendCustomMessage("resetView", TRUE)
  })
}

shinyApp(ui = ui, server = server)