<apply template="base">
  
  <div class="row">
    <div class="col s8"> 
      <h3 class="color-prim-3"><cdaTitle /></h3>
      <blockquote>
        <strong>Document dated: </strong><cdaDate /><br>
        <strong>Document author: </strong><cdaAuthors><prefix /> <given /> <family /> <suffix /></cdaAuthors>
      </blockquote>
    </div>
    <div class="col s4">
      <div class="card-panel">
        <h5 class="color-prim-4">Patient</h5>
        <cdaPatients>
          <strong>Name: </strong><prefix /> <given /> <family /> <suffix /><br>
          <strong>Date of birth: </strong><pBirth /><br>
          <strong>Gender: </strong><pGender /><br>
          <strong>Race: </strong><pRace /> <strong>Ethnos: </strong><pEthnos /><br>
          <br>
          <strong>Contacts information:</strong><br>
          <streetAddressLine /> <state /> <br>
          <country /> <postalCode /><br>
          <pTele />
        </cdaPatients>
      </div>
    </div>
  </div>    

  <cdaSections>
    <ul class="collapsible" data-collapsible="expandable">
    <cdaSection />
  </ul>
  </cdaSections>
  
</apply>
